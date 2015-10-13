open Batteries

let indent = ref 0
let emit fmt =
  let indent = String.make (!indent * 2) ' ' in
  Printf.printf ("%s" ^^ fmt ^^ "\n") indent

let log fmt =
  let indent = String.make (!indent * 2) ' ' in
  Printf.ifprintf "" ("%s" ^^ fmt ^^ "\n") indent

module Field = struct
  type t = { name: string; tpe: string } (* Reserved flag *)
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
  { Field.name; tpe }

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

let emit_method class_index { Method.name;
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
  begin
    match arguments with
    | [] ->
      emit "type t = unit";
      emit "let apply f _ = f"; (* Do we need unit on nil? *)
      emit "let make = ()" (* Do we need unit on nil? *)
    | _ ->
       arguments
       |> List.map
            (fun t ->
             Printf.sprintf
               "%s: %s"
               (bind_name t.Field.name)
               (bind_name t.Field.tpe)
            )
       |> String.concat "; "
       |> emit "type t = { %s }";
       let names = List.map (fun t -> bind_name t.Field.name) arguments in
       emit "let apply f { %s } = f %s"
         (String.concat "; " names) (String.concat " " names);
       emit "let make %s = { %s }"
         (String.concat " " names) (String.concat "; " names) ;
  end;
  emit "let def = ((%d, %d), spec, make, apply)" class_index index;
  emit "(* Name %s *)" name;
  emit "(* Server %b *)" server;
  emit "(* Client %b *)" client;
  emit "(* Synchronous %b *)" synchronous;
  emit "(* Response: [%s] *)" (response |> String.concat ";");

  let response = List.map variant_name response in
  if List.length response = 1 && ((synchronous && response != []) || not synchronous)  then begin
    if client then
      emit "let reply : C.t -> (t -> %s.t D.t) -> unit D.t = reply%d def %s"
        (List.hd response)
        (List.length response)
        (response |> List.map (Printf.sprintf "%s.def") |> String.concat " ");
    if server then
      emit "let request : C.t -> t -> %s.t D.t = request%d def %s"
        (List.hd response)
        (List.length response)
        (response |> List.map (Printf.sprintf "%s.def") |> String.concat " ");
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
    emit_method index { Method.name = "payload";
                        arguments = content;
                        response = [];
                        content = false;
                        index = 0;
                        synchronous = false;
                        server=false;
                        client=false
                      };

  List.iter (emit_method index) methods;

  decr indent;
  emit "end";
  ()

let _ =
  let xml = Xml.parse_file Sys.argv.(1) in
  emit "(***********************************)";
  emit "(* AUTOGENERATED FILE: DO NOT EDIT *)";
  emit "(* %s %s *)" Sys.argv.(0) Sys.argv.(1);
  emit "(***********************************)";
  emit "";
  emit "";
  emit "open Types";
  emit "open Util";
  emit "module C = Channel";
  emit "module D = Async.Std.Deferred";
  let tree = xml |> parse_amqp |> emit_domains in
  emit_constants tree;
  List.iter (function Class x -> emit_class x | _ -> ()) tree;
