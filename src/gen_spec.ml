let str fmt = Printf.sprintf fmt

let indent = ref 0
let emit fmt =
  assert (!indent >= 0);
  let indent = String.make (!indent * 2) ' ' in
  Printf.printf ("%s" ^^ fmt ^^ "\n") indent

let emit_doc = function
  | Some doc ->
    emit "";
    emit "(** %s *)" doc
  | None -> ()

let log fmt =
  let indent = String.make (!indent * 2) ' ' in
  Printf.ifprintf "" ("%s" ^^ fmt ^^ "\n") indent

module Field = struct
  type t = { name: string; tpe: string; reserved: bool; doc: string option }
end
module Constant = struct
  type t = { name: string; value: int; doc: string option }
end
module Domain = struct
  type t = { name: string; amqp_type: string; doc: string option }
end
module Method = struct
  type t = { name: string; arguments: Field.t list;
             response: string list; content: bool;
             index: int; synchronous: bool; server: bool; client: bool;
             doc: string option
           }
end
module Class = struct
  type t = { name: string; content: Field.t list; index: int;
             methods: Method.t list; doc: string option }
end

type elem =
  | Constant of Constant.t
  | Domain of Domain.t
  | Class of Class.t

let blanks = Str.regexp "[ \t\n]+"
let rec doc = function
  | Xml.Element("doc", [], [ Xml.PCData doc]) :: _ -> Some (String.trim (Str.global_replace blanks " " doc))
  | _ :: xs -> doc xs
  | [] -> None

let rec map_elements tpe f = function
  | [] -> []
  | Xml.Element(t, a, n) :: xs when t = tpe -> f a n :: map_elements tpe f xs
  | _ :: xs -> map_elements tpe f xs

let parse_field attrs nodes =
  ignore nodes;
  let name =
    List.assoc "name" attrs
    |> (function "type" -> "amqp_type" | s -> s)
  in
  log "Parsing field %s" name;
  let tpe =
    match List.assoc "domain" attrs with
    | d -> d
    | exception Not_found -> List.assoc "type" attrs
  in

  let reserved = List.mem ("reserved", "1")  attrs in
  { Field.name; tpe; reserved; doc = doc nodes }

let parse_constant attrs nodes =
  let name = List.assoc "name" attrs in
  log "Parsing constant %s" name;

  let value = List.assoc "value" attrs |> int_of_string in
  Constant { Constant.name; value; doc = doc nodes }

let parse_domain attrs nodes =
  ignore nodes;
  let name = List.assoc "name" attrs in
  log "Parsing domain %s" name;

  let amqp_type = List.assoc "type" attrs in
  Domain { Domain.name; amqp_type; doc = doc nodes}

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

  let synchronous = List.mem ("synchronous", "1") attrs in
  let content = List.mem ("content", "1") attrs in
  let arguments = map_elements "field" parse_field nodes in

  let chassis =
    List.fold_left
      (fun acc -> function Xml.Element ("chassis", attrs, _) -> acc @ attrs | _ -> acc) [] nodes
    |> List.filter (fun (n, _) -> n = "name")
    |> List.map snd
  in
  let client = List.mem "client" chassis in
  let server = List.mem "server" chassis in
  decr indent;
  { Method.name; arguments; response; content; index; synchronous;
    client; server; doc = doc nodes }

let parse_class attrs nodes =
  (* All field nodes goes into content *)
  let name = List.assoc "name" attrs in
  log "Parsing class %s" name;
  incr indent;

  let index = List.assoc "index" attrs |> int_of_string in
  let fields = map_elements "field" parse_field nodes in
  let methods = map_elements "method" parse_method nodes in
  decr indent;
  Class { Class.name; index; content=fields; methods; doc = doc nodes }

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
  String.map (function '-' -> '_' | c -> Char.lowercase c) str

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
      | Domain {Domain.name; amqp_type; doc} when name <> amqp_type ->
        Hashtbl.add domains name (amqp_type, doc)
      | _ -> ()) tree;

  emit "(* Domains *)";
  Hashtbl.iter (fun d (t, doc) ->
      emit_doc doc;
      emit "type %s = %s" (bind_name d) (bind_name t);
    ) domains;

  emit "module Internal_alias = struct";
  incr indent;
  Hashtbl.iter (fun d (t, _) ->
      emit "let %s = %s" (bind_name d) (variant_name t);
    ) domains;
  decr indent;
  emit "end";

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
  List.fold_left (fun acc e -> match map e with Some x -> x :: acc | None -> acc) [] tree

let emit_constants tree =
  emit "(* Constants *)";
  List.iter
    (function Constant { Constant.name; value; doc } ->
      emit_doc doc;
      emit "let %s = %d" (bind_name name) value | _ -> ())
    tree

let spec_str arguments =
  arguments
  |> List.map (fun t -> t.Field.tpe)
  |> fun a -> List.append a ["Nil"]
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
      doc;
    } =

  emit_doc doc;
  emit "module %s = struct" (variant_name name);
  incr indent;
  let t_args =
    arguments
    |> List.filter (fun t -> not t.Field.reserved)
  in
  let option = if is_content then " option" else "" in
  let doc_str = function
    | None -> ""
    | Some doc -> "(** " ^ doc ^ " *)"
  in
  let t_spec, t_args = match t_args with
    | [] -> "unit", "()"
    | args ->
      let a = List.map (fun t -> (bind_name t.Field.name), (bind_name t.Field.tpe) ^ option, doc_str t.Field.doc) args in
      (
        a |> List.map (fun (a, b, doc) -> a ^ ": " ^ b ^ "; " ^ doc) |> String.concat "\n" |> str "{ %s }",
        a |> List.map (fun (a, _, _) -> a) |> String.concat "; " |> str "{ %s }"
      )
  in
  let names =
    arguments
    |> List.map (function t when t.Field.reserved -> "_" | t -> bind_name t.Field.name)
  in
  let values =
    arguments
    |> List.map (function
        | t when t.Field.reserved ->
          "(reserved_value " ^ t.Field.tpe ^ ")"
        | t -> bind_name t.Field.name)
    |> String.concat " "
  in

  emit "type t = %s" t_spec;
  emit "module Internal = struct";
  incr indent;
  emit "open !Internal_alias";

  if is_content then
    emit "open Amqp_types.Content"
  else
    emit "open Amqp_types.Spec";

  emit "let spec = %s" (spec_str arguments);

  emit "let make %s = %s" (String.concat " " names) t_args;
  emit "let apply f %s = f %s" t_args values;
  emit "let def = ((%d, %d), spec, make, apply)" class_index index;


  begin match is_content, content with
    | false, false  ->
      emit "let write = write_method def";
      emit "let read = read_method def"
    | false, true ->
      emit "let write = write_method_content def Content.Internal.def";
      emit "let read = read_method_content def Content.Internal.def"
    | true, _ ->
      ()
  end;

  decr indent;
  emit "end";
  let inames = List.filter ((<>) "_") names in
  begin match is_content with
    | true ->
      emit "let init %s () = Internal.make %s" (List.map (fun n -> "?" ^ n) inames |> String.concat " ") (String.concat " " inames)
    | false ->
      emit "let init %s () = Internal.make %s" (List.map (fun n -> "~" ^ n) inames |> String.concat " ") (String.concat " " inames)
  end;



  let response = List.map variant_name response in
  if List.length response >= 0 && ((synchronous && response != []) || not synchronous)  then begin
    let id r =
      if List.length response > 1 then
        "(fun m -> `" ^ r ^ " m)"
      else
        ""
    in
    if client then
      emit "let reply = reply%d Internal.read %s"
        (List.length response)
        (response |> List.map (fun s -> Printf.sprintf "%s.Internal.write %s" s (id s)) |> String.concat " ");
    if server then
      emit "let request = request%d Internal.write %s"
        (List.length response)
        (response |> List.map (fun s -> Printf.sprintf "%s.Internal.read %s" s (id s)) |> String.concat " ");
  end;
  decr indent;
  emit "end";
  ()


let emit_class { Class.name; content; index; methods; doc } =
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
  emit_doc doc;
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
              client=false;
              doc = None;
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
  emit "(** Low level protocol description *)";
  emit "(***********************************)";
  emit "(* AUTOGENERATED FILE: DO NOT EDIT *)";
  emit "(* %s %s *)" Sys.argv.(0) Sys.argv.(1);
  emit "(***********************************)";
  emit "";
  emit "";
  emit "open Amqp_types";
  emit "open Amqp_util";
  let tree = xml |> parse_amqp |> emit_domains in
  emit_constants tree;
  List.iter (function Class x -> emit_class x | _ -> ()) tree;
  emit_printer tree;
  (* assert (!indent = 0);*)
  ()
