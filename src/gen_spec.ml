open Batteries

let str fmt = Printf.sprintf fmt

let indent = ref 0
let emit fmt =
  assert (!indent >= 0);
  let indent = String.make (!indent * 2) ' ' in
  Printf.printf ("%s" ^^ fmt ^^ "\n") indent

let log fmt =
  let indent = String.make (!indent * 2) ' ' in
  Printf.ifprintf "" ("%s" ^^ fmt ^^ "\n") indent

module Field = struct
  type t = { name: string; tpe: string; reserved: bool }
end
module Constant = struct
  type t = { name: string; value: int }
end
module Domain = struct
  type t = { name: string; amqp_type: string; (* asserts *) }
end
module Method = struct
  type t = { name: string; arguments: Field.t list;
             response: string list; content: bool;
             index: int; synchronous: bool; server: bool; client: bool}
end
module Class = struct
  type t = { name: string; content: Field.t list; index: int; methods: Method.t list }
end

type elem =
  | Constant of Constant.t
  | Domain of Domain.t
  | Class of Class.t

let rec map_elements tpe f = function
  | [] -> []
  | Xml.Element(t, a, n) :: xs when t = tpe -> f a n :: map_elements tpe f xs
  | _ :: xs -> map_elements tpe f xs

let parse_field attrs nodes =
  (* Todo: Handle asserts *)
  ignore nodes;
  let name =
    List.assoc "name" attrs
    |> (function "type" -> "amqp_type" | s -> s)
  in
  log "Parsing field %s" name;
  let tpe =
    match List.Exceptionless.assoc "domain" attrs with
    | Some d -> d
    | None -> List.assoc "type" attrs
  in

  let open Option.Infix in
  let reserved = List.Exceptionless.assoc "reserved" attrs |> ((=) (Some "1")) in
  { Field.name; tpe; reserved }

let parse_constant attrs nodes =
  assert (nodes = []);
  let name = List.assoc "name" attrs in
  log "Parsing constant %s" name;

  let value = List.assoc "value" attrs |> int_of_string in
  Constant { Constant.name; value }

let parse_domain attrs nodes =
  (* TODO - Add assertions for the function *)
  ignore nodes;
  let name = List.assoc "name" attrs in
  log "Parsing domain %s" name;

  let amqp_type = List.assoc "type" attrs in
  Domain { Domain.name; amqp_type}

let parse_method attrs nodes =
  let name = List.assoc "name" attrs in
  log "Parsing method %s" name;
  incr indent;
  let index = List.assoc "index" attrs |> int_of_string in
  let response =
    nodes
    |> List.filter (function Xml.Element ("response", _, _) -> true | _ -> false)
    |> List.map (function Xml.Element ("response", attrs, []) -> List.assoc "name" attrs | _ -> failwith "Error parsing response")
  in

  let synchronous = List.Exceptionless.assoc "synchronous" attrs |> Option.map_default ((=) "1") false in
  let content = List.Exceptionless.assoc "content" attrs |> Option.map_default ((=) "1") false in
  let arguments = map_elements "field" parse_field nodes in

  let chassis = List.filter_map (function
      | Xml.Element ("chassis", attrs, _) ->
        List.Exceptionless.assoc "name" attrs
      | _ -> None) nodes
  in
  let client = List.mem "client" chassis in
  let server = List.mem "server" chassis in
  decr indent;
  { Method.name; arguments; response; content; index; synchronous; client; server }

let parse_class attrs nodes =
  (* All field nodes goes into content *)
  let name = List.assoc "name" attrs in
  log "Parsing class %s" name;
  incr indent;

  let index = List.assoc "index" attrs |> int_of_string in
  let fields = map_elements "field" parse_field nodes in
  let methods = map_elements "method" parse_method nodes in
  decr indent;
  Class { Class.name; index; content=fields; methods }

let parse = function
  | Xml.PCData _ -> failwith "pc data not supported"
  | Xml.Element ("constant", attrs, nodes) -> parse_constant attrs nodes
  | Xml.Element ("domain", attrs, nodes) -> parse_domain attrs nodes
  | Xml.Element ("class", attrs, nodes) -> parse_class attrs nodes
  | Xml.Element (t, _, _) -> failwith ("Unknown type: " ^ t)

let parse_amqp = function
  | Xml.Element ("amqp", _attrs, nodes) ->
      List.map parse nodes
  | _ -> failwith "Error parsing"

let bind_name str =
  String.nreplace ~str ~sub:"-" ~by:"_" |> String.lowercase

let variant_name str =
  bind_name str
  |> String.capitalize

let pvariant_name str =
  "`" ^ (variant_name str)



let rec print = function
  | Xml.Element (n, attrs, nodes) ->
      let attrs = List.map (fun (a,b) -> a ^ " = " ^ b) attrs
                  |> String.concat ", "
      in
      log "%s: %s" n attrs;
      incr indent;
      List.iter print nodes;
      decr indent;
  | _ -> ()


(* Remove domains *)
let emit_domains tree =
  let domains = Hashtbl.create 0 in
  List.iter (function
      | Domain {Domain.name; amqp_type} when name <> amqp_type ->
        Hashtbl.add domains name amqp_type
      | _ -> ()) tree;

  emit "(* Domains *)";
  Hashtbl.iter (fun d t -> emit "let %s = %s" (bind_name d) (variant_name t)) domains;
  emit "(* Aliases *)";
  Hashtbl.iter (fun d t -> emit "type %s = %s" (bind_name d) (bind_name t)) domains;

  (* Alter the tree *)
  let replace lst =
    let open Field in
    List.map (fun t ->
        let tpe = match Hashtbl.mem domains t.tpe with
          | true -> bind_name t.tpe
          | false -> variant_name t.tpe
        in
        { t with tpe }) lst
  in
  let map = function
    | Domain _ -> None
    | Constant c -> Some (Constant c)
    | Class ({ Class.content; methods; _ } as c) ->
        let methods =
          List.map
            (function {Method.arguments; _ } as m -> { m with Method.arguments = replace arguments })
            methods
        in
        Some (Class { c with Class.methods; content = replace content })
  in
  List.filter_map map tree

let emit_constants tree =
  emit "(* Constants *)";
  List.iter
    (function Constant { Constant.name; value } -> emit "let %s = %d" (bind_name name) value | _ -> ())
    tree

let spec_str arguments =
  arguments
  |> List.map (fun t -> t.Field.tpe)
  |> flip List.append ["Nil"]
  |> String.concat " :: "

let emit_method ?(is_content=false) class_index
    { Method.name;
      arguments;
      response;
      content;
      index;
      synchronous;
      client;
      server;
    } =
  ignore content; ignore index; ignore server; ignore client; ignore class_index;
  emit "module %s = struct" (variant_name name);
  incr indent;
  emit "let spec = %s" (spec_str arguments);
  let t_args =
    arguments
    |> List.filter (fun t -> not t.Field.reserved)
  in
  let t_spec, t_args = match t_args with
    | [] -> "unit", "()"
    | args ->
      let a = List.map (fun t -> (bind_name t.Field.name), (bind_name t.Field.tpe)) args in
      (
        a |> List.map (fun (a, b) -> a ^ ": " ^ b) |> String.concat "; " |> str "{ %s }",
        a |> List.map (fun (a, _) -> a) |> String.concat "; " |> str "{ %s }"
      )
  in
  let names =
    arguments
    |> List.map (function t when t.Field.reserved -> "_" | t -> bind_name t.Field.name)
    |> String.concat " "
  in
  let values =
    arguments
    |> List.map (function
        | t when t.Field.reserved ->
          "(reserved_value " ^ t.Field.tpe ^ ")"
        | t -> bind_name t.Field.name)
    |> String.concat " "
  in
  let frame_type =
    match is_content with
    | true -> "Amqp_framing.Content"
    | false -> "Amqp_framing.Method"
  in

  emit "type t = %s" t_spec;
  emit "let make %s = %s" names t_args;
  emit "let apply f %s = f %s" t_args values;

  emit "let def = (%s, (%d, %d), spec, make, apply)" frame_type class_index index;

  let response = List.map variant_name response in
  if List.length response >= 0 && ((synchronous && response != []) || not synchronous)  then begin
    let id r =
      if List.length response > 1 then
        "(fun m -> `" ^ r ^ " m)"
      else
        ""
    in
    if client then
      emit "let reply = reply%d def %s"
        (List.length response)
        (response |> List.map (fun s -> Printf.sprintf "%s.def %s" s (id s)) |> String.concat " ");
    if server then
      emit "let request = request%d def %s"
        (List.length response)
        (response |> List.map (fun s -> Printf.sprintf "%s.def %s" s (id s)) |> String.concat " ");
  end;
  decr indent;
  emit "end";
  ()


let emit_class { Class.name; content; index; methods } =
  (* Reorder modules based on dependencies *)
  let rec reorder methods =
    let rec move_down = function
      | { Method.response; _} as m :: x :: xs when
          List.exists (fun r -> List.exists (fun {Method.name; _} -> name = r) (x :: xs)) response -> x :: move_down (m :: xs)
      | x :: xs -> x :: move_down xs
      | [] -> []
    in
    let ms = move_down methods in
    if ms = methods then ms
    else reorder ms
  in
  let methods = reorder methods in
  emit "module %s = struct" (variant_name name);
  incr indent;

  if (content != []) then
    emit_method ~is_content:true
      index { Method.name = "content";
              arguments = content;
              response = [];
              content = false;
              index = 0; (* must be zero *)
              synchronous = false;
              server=false;
              client=false
            };

  List.iter (emit_method index) methods;
  emit "let method_to_string = function";
  incr indent;
  List.iter (fun {Method.name; index; _} -> emit "| %d -> \"%s\"" index name) methods;
  emit "| n -> Printf.sprintf \"<%%d>\" n";
  decr indent;
  decr indent;
  emit "end";
  ()

let emit_printer tree =
  emit "module Printer = struct";
  incr indent;
  emit "let id_to_string (cid, mid) =";
  incr indent;
  emit "match cid with";
  incr indent;
  List.iter (function
      | Class {Class.name; index; _} ->
        emit "| %d -> \"%s\" ^ \", \" ^(%s.method_to_string mid)" index name (variant_name name)
      | _ -> ()
    ) tree;
  emit "| _ -> Printf.sprintf \"<%%d>, <%%d>\" mid cid";
  decr indent;
  decr indent;
  decr indent;
  emit "end";
  ()

let () =
  let xml = Xml.parse_file Sys.argv.(1) in
  emit "(***********************************)";
  emit "(* AUTOGENERATED FILE: DO NOT EDIT *)";
  emit "(* %s %s *)" Sys.argv.(0) Sys.argv.(1);
  emit "(***********************************)";
  emit "";
  emit "";
  emit "open Amqp_types";
  emit "open Amqp_types.Spec";
  emit "open Amqp_util";
  emit "module C = Amqp_channel";
  emit "module D = Async.Std.Deferred";
  let tree = xml |> parse_amqp |> emit_domains in
  emit_constants tree;
  List.iter (function Class x -> emit_class x | _ -> ()) tree;
  emit_printer tree;
  (* assert (!indent = 0);*)
  ()
