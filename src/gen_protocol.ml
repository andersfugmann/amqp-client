open Batteries

let indent = ref 0
let emit fmt =
  let indent = String.make (!indent * 2) ' ' in
  Printf.printf ("%s" ^^ fmt ^^ "\n") indent

let log fmt =
  let indent = String.make (!indent * 2) ' ' in
  Printf.ifprintf "" ("%s" ^^ fmt ^^ "\n") indent

module Field = struct
  type tpe = Domain of string | Type of string
  type t = { name: string; tpe: tpe } (* Reserved flag *)
end
module Constant = struct
  type t = { name: string; value: int }
end
module Domain = struct
  type t = { name: string; amqp_type: string; (* asserts *) }
end
module Method = struct
  type t = { name: string; arguments: Field.t list; response: string option; content: bool; index : int }
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
    | Some d -> Field.Domain d
    | None -> Field.Type (List.assoc "type" attrs)
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
    |> List.Exceptionless.find (function Xml.Element ("response", _, _) -> true | _ -> false)
    |> Option.map (function Xml.Element ("response", attrs, []) -> List.assoc "name" attrs | _ -> failwith "Error parsing response")
  in
  (*
  let synchronous = List.Exceptionless.assoc "synchronous" attrs |> Option.map_default ((=) "1") false in
  assert (not synchronous || response <> None);
  *)
  let content = List.Exceptionless.assoc "content" attrs |> Option.map_default ((=) "1") false in
  let arguments = map_elements "field" parse_field nodes in
  decr indent;
  { Method.name; arguments; response; content; index }

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
  String.nreplace ~str ~sub:"-" ~by:"_"

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



(*********** Ok. Now comes the funny part. We have our AST ******)
(* Remove domains *)
let inline_domains tree =
  let domains = Hashtbl.create 0 in
  List.iter (function Domain {Domain.name; amqp_type} -> Hashtbl.add domains name amqp_type | _ -> ()) tree;
  (* Alter the tree *)
  let replace lst =
    List.map (function { Field.name; tpe = Field.Domain d } -> {Field.name; tpe = Field.Type (Hashtbl.find domains d)}
                     | { Field.name; tpe = Field.Type t } -> { Field.name; tpe = Field.Type t}) lst
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

let emit_method class_index { Method.name; arguments; response = _; content; index } =
  emit "module %s = struct" (variant_name name);
  incr indent;

  emit "type t = %s" (* Replace with unit type if no arguments *)
    (arguments
     |> List.map (function {Field.name; tpe = Field.Type t} -> Printf.sprintf "%s: %s" (bind_name name) (bind_name t)
                         | {Field.name; tpe = Field.Domain _} -> failwith ("Domain present: " ^ name))
     |> String.concat "; "
     |> (function "" -> "unit" | s -> "{ " ^ s ^ " }" )
    );

  emit "let encode (arg : t) %s = " (if content then "(payload: Payload.t)" else "");
  incr indent;
  if arguments = [] then emit "ignore arg;";
  emit "[%s] %s"
    (arguments
     |> List.map (function {Field.name; tpe = Field.Type t} -> Printf.sprintf "Param.%s arg.%s"  (variant_name t) (bind_name name)
                         | {Field.name; tpe = Field.Domain _} -> failwith ("Domain present: " ^ name))
     |> String.concat "; ")
    (if content then "@ (Payload.encode payload)" else "");
  decr indent;


  emit "let format = [%s] %s"
    (arguments
     |> List.map (function {Field.name = _; tpe = Field.Type t} -> Printf.sprintf "Type.%s" (variant_name t)
                         | {Field.name; tpe = Field.Domain _} -> failwith ("Domain present: " ^ name))
     |> String.concat "; ")
    (if content then "@ (Payload.format)" else "");

  emit "let decode args = %s args" (pvariant_name name);

  emit "let make (arg : t) %s= { major = %d; minor = %d; params = encode arg %s}"
    (if content then "(payload: Payload.t) " else "")
    class_index index
    (if content then "payload " else "");
  decr indent;
  emit "end";
  ()

let emit_class { Class.name; content; index; methods } =
  emit "module %s = struct" (variant_name name);
  incr indent;

  emit_method index { Method.name = "payload"; arguments = content; response = None; content = false; index = 0 };

  List.iter (emit_method index) methods;

  (* Emit decode dispatch *)
  emit "let decode = function";
  incr indent;
  List.iter (fun {Method.name; index; _} -> emit "| %d -> (%s.format, %s.decode)" index (variant_name name) (variant_name name)) methods;
  emit "| d -> raise (Unknown_minor d)";
  decr indent;


  decr indent;
  emit "end";
  ()

let _ =
  let xml = Xml.parse_file Sys.argv.(1) in
  print xml;
  let tree = xml |> parse_amqp |> inline_domains in
  emit "open Amqp_types";
  emit "let (@) = Pervasives.(@)";
  emit_constants tree;
  List.iter (function Class x -> emit_class x | _ -> ()) tree;


  (* emit global decode dispatch *)
  emit "let decode = function";
  incr indent;
  tree
  |> List.filter_map (function Class {Class.name; index; _} -> Some (name, index) | _ -> None)
  |> List.iter (fun (name, index) -> emit "| %d -> %s.decode" index (variant_name name));
  emit "| d -> raise (Unknown_major d)";
  decr indent;
