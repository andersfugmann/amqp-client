open Printf
module List = ListLabels
let indent = ref 0
let emit_location = ref true

let option_map ~f = function
  | Some v -> f v
  | None -> None

let option_iter ~f = function
  | Some v -> f v
  | None -> ()

let emit_loc loc =
  match !emit_location with
  | true ->
    let indent = String.make (!indent * 2) ' ' in
    printf "%s(* %s:%d *)\n" indent __FILE__ loc
  | false ->
    printf "# %d \"%s\"\n" loc __FILE__

let emit ?loc fmt =
  option_iter ~f:emit_loc loc;
  assert (!indent >= 0);
  let indent = String.make (!indent * 2) ' ' in
  (* Get last location *)
  printf ("%s" ^^ fmt ^^ "\n") indent

let emit_doc = function
  | Some doc ->
    emit "";
    emit "(** %s *)" doc
  | None -> ()


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
let doc xml =
  try
    Ezxmlm.member "doc" xml
    |> Ezxmlm.data_to_string
    |> (fun x -> Some x)
  with
  | Ezxmlm.Tag_not_found _ -> None

let parse_field (attrs, nodes) =
  (* Only look at the attributes *)
  ignore nodes;
  let name =
    match Ezxmlm.get_attr "name" attrs with
    | "type" -> "amqp_type"
    | name -> name
  in
  let tpe =
    match Ezxmlm.get_attr "domain" attrs with
    | d -> d
    | exception Not_found -> Ezxmlm.get_attr "type" attrs
  in

  let reserved = Ezxmlm.mem_attr "reserved" "1" attrs in
  { Field.name; tpe; reserved; doc = doc nodes }

let parse_constant (attrs, nodes) =
  let name = Ezxmlm.get_attr "name" attrs in
  let value = Ezxmlm.get_attr "value" attrs |> int_of_string in
  Constant { Constant.name; value; doc = doc nodes }

let parse_domain (attrs, nodes) =
  ignore nodes;
  let name = Ezxmlm.get_attr "name" attrs in
  let amqp_type = Ezxmlm.get_attr "type" attrs in
  Domain { Domain.name; amqp_type; doc = doc nodes}

let parse_method (attrs, nodes) =
  let name = Ezxmlm.get_attr "name" attrs in
  incr indent;
  let index = Ezxmlm.get_attr "index" attrs |> int_of_string in
  let response =
    Ezxmlm.members_with_attr "response" nodes
    |> List.map ~f:(fun (attrs, _) -> Ezxmlm.get_attr "name" attrs)
  in

  let synchronous =
    match Ezxmlm.get_attr "synchronous" attrs with
    | "1" -> true
    | _ -> false
    | exception Not_found -> false
  in
  let content =
    match Ezxmlm.get_attr "content" attrs with
    | "1" -> true
    | _ -> false
    | exception Not_found -> false
  in
  let arguments = Ezxmlm.members_with_attr "field" nodes |> List.map ~f:parse_field in

  let chassis =
    Ezxmlm.members_with_attr "chassis" nodes
    |> List.map ~f:(fun (attrs, _) -> Ezxmlm.get_attr "name" attrs)
  in
  let client = List.mem "client" ~set:chassis in
  let server = List.mem "server" ~set:chassis in
  decr indent;
  { Method.name; arguments; response; content; index; synchronous;
    client; server; doc = doc nodes }

let parse_class (attrs, nodes) =
  (* All field nodes goes into content *)
  let name = Ezxmlm.get_attr "name" attrs in
  incr indent;
  let index = Ezxmlm.get_attr "index" attrs |> int_of_string in
  let fields = Ezxmlm.members_with_attr "field" nodes |> List.map ~f:parse_field in
  let methods = Ezxmlm.members_with_attr "method" nodes |> List.map ~f:parse_method in
  decr indent;
  Class { Class.name; index; content=fields; methods; doc = doc nodes }

let parse = function
  | `Data _ -> None
  | `El (((_, "constant"), attrs), nodes) -> Some (parse_constant (attrs, nodes))
  | `El (((_, "domain"), attrs), nodes) -> Some (parse_domain (attrs, nodes))
  | `El (((_, "class"), attrs), nodes) -> Some (parse_class (attrs, nodes))
  | `El (((_, name), _), _) -> failwith ("Unknown type: " ^ name)

let parse_amqp xml =
  Ezxmlm.member "amqp" xml
  |> List.map ~f:parse
  |> List.fold_left ~f:(fun acc -> function None -> acc | Some v -> v :: acc) ~init:[]
  |> List.rev

let bind_name str =
  String.map (function '-' -> '_' | c -> Char.lowercase_ascii c) str

let variant_name str =
  bind_name str
  |> String.capitalize_ascii

let pvariant_name str =
  "`" ^ (variant_name str)

(* Remove domains *)
let emit_domains tree =
  let domains = Hashtbl.create 0 in
  List.iter ~f:(function
      | Domain {Domain.name; amqp_type; doc} when name <> amqp_type ->
        Hashtbl.add domains name (amqp_type, doc)
      | _ -> ()
    ) tree;

  emit "(* Domains *)";
  Hashtbl.iter (fun d (t, doc) ->
      emit_doc doc;
      emit ~loc:__LINE__ "type %s = %s" (bind_name d) (bind_name t);
    ) domains;

  emit "";
  emit "(**/**)";
  emit ~loc:__LINE__ "module Internal_alias = struct";
  incr indent;

  Hashtbl.iter (fun d (t, _) ->
      emit "let %s = %s" (bind_name d) (variant_name t);
    ) domains;
  decr indent;
  emit "end";
  emit "(**/**)";
  emit "";

  (* Alter the tree *)
  let replace lst =
    let open Field in
    List.map ~f:(fun t ->
        let tpe = match Hashtbl.mem domains t.tpe with
          | true -> bind_name t.tpe
          | false -> variant_name t.tpe
        in
        { t with tpe }
      ) lst
  in
  let map = function
    | Domain _ -> None
    | Constant c -> Some (Constant c)
    | Class ({ Class.content; methods; _ } as c) ->
        let methods =
          List.map ~f:(function {Method.arguments; _ } as m ->
              { m with Method.arguments = replace arguments }
            ) methods
        in
        Some (Class { c with Class.methods; content = replace content })
  in
  List.fold_left ~f:(fun acc e -> match map e with Some x -> x :: acc | None -> acc) ~init:[] tree

let emit_constants tree =
  emit "(* Constants *)";
  List.iter ~f:(function Constant { Constant.name; value; doc } ->
      emit_doc doc;
      emit ~loc:__LINE__ "let %s = %d" (bind_name name) value | _ -> ()
    ) tree

let emit_class_index tree =
  emit "(* Class index *)";
  let idx = ref 0 in
  emit ~loc:__LINE__ "let index_of_class = function";
  incr indent;
  List.iter ~f:(function Class { Class.index; _ } -> emit "| %d -> %d" index !idx; incr idx | _ -> ()) tree;
  emit "| _ -> failwith \"Unknown class\"";
  decr indent;
  emit ~loc:__LINE__ "let classes = %d" !idx

let emit_method_index tree =
  emit "(* Class - Method index *)";
  let idx = ref 0 in
  emit ~loc:__LINE__ "let index_of_class_method = function";
  incr indent;
  List.iter ~f:(function
      | Class { Class.index; methods; _ } ->
        emit "| %d -> begin function" index;
        incr indent;
        List.iter ~f:(fun { Method.index; _ } ->
            emit "| %d -> %d" index !idx;
            incr idx
          ) methods;
        emit "| _ -> failwith \"Unknown method\"";
        emit "end";
        decr indent;
      | _ -> ()
    ) tree;
  emit "| _ -> failwith \"Unknown class\"";
  decr indent;
  emit ~loc:__LINE__ "let methods = %d" !idx


let spec_str arguments =
  arguments
  |> List.map ~f:(fun t -> t.Field.tpe)
  |> fun a -> List.append a ["[]"]
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
  emit ~loc:__LINE__ "module %s = struct" (variant_name name);
  incr indent;
  let t_args =
    arguments
    |> List.filter ~f:(fun t -> not t.Field.reserved)
  in
  let option = if is_content then " option" else "" in
  let doc_str = function
    | None -> ""
    | Some doc -> "(** " ^ doc ^ " *)"
  in
  let types = List.map ~f:(fun t -> (bind_name t.Field.name), (bind_name t.Field.tpe) ^ option, doc_str t.Field.doc) t_args in

  let t_args = match types with
    | [] -> "()"
    | t -> List.map ~f:(fun (a, _, _) -> a) t |> String.concat "; " |> sprintf "{ %s }"
  in
  let names =
    arguments
    |> List.map ~f:(function t when t.Field.reserved -> "_" | t -> bind_name t.Field.name)
  in
  let values =
    arguments
    |> List.map ~f:(function
        | t when t.Field.reserved ->
          "(reserved_value " ^ t.Field.tpe ^ ")"
        | t -> bind_name t.Field.name
      )
    |> String.concat " "
  in

  (match types with
   | [] -> emit ~loc:__LINE__ "type t = unit"
   | t ->
     emit ~loc:__LINE__ "type t = {";
     incr indent;
     List.iter ~f:(fun (a, b, doc) -> emit "%s: %s; %s" a b doc) t;
     decr indent;
     emit "}");
  emit "";
  emit "(**/**)";
  emit ~loc:__LINE__ "module Internal = struct";
  incr indent;
  emit "open Internal_alias [@@warning \"-33\"]";

  if is_content then
    emit "open Protocol.Content"
  else
    emit "open Protocol.Spec";


  emit_loc __LINE__;
  emit "let spec = %s" (spec_str arguments);
  emit "let make %s = %s" (String.concat " " names) t_args;
  emit "let apply f %s = f %s" t_args values;
  emit "let def = ((%d, %d), spec, make, apply)" class_index index;

  begin match is_content, content with
    | false, false  ->
      emit ~loc:__LINE__ "let write = write_method def";
      emit ~loc:__LINE__ "let read = read_method def"
    | false, true ->
      emit ~loc:__LINE__ "let write = write_method_content def Content.Internal.def";
      emit ~loc:__LINE__ "let read = read_method_content def Content.Internal.def"
    | true, _ ->
      ()
  end;

  decr indent;
  emit "end";
  emit "(**/**)";
  emit "";

  let inames = List.filter ~f:((<>) "_") names in
  begin match is_content with
    | true ->
      emit ~loc:__LINE__ "let init %s () = Internal.make %s" (List.map ~f:(fun n -> "?" ^ n) inames |> String.concat " ") (String.concat " " inames)
    | false ->
      emit ~loc:__LINE__ "let init %s () = Internal.make %s" (List.map ~f:(fun n -> "~" ^ n) inames |> String.concat " ") (String.concat " " inames)
  end;



  let response = List.map ~f:variant_name response in
  if List.length response >= 0 && ((synchronous && response != []) || not synchronous)  then begin
    let id r =
      if List.length response > 1 then
        "(fun m -> `" ^ r ^ " m)"
      else
        ""
    in
    if client then
      emit ~loc:__LINE__ "let reply = reply%d Internal.read %s"
        (List.length response)
        (response |> List.map ~f:(fun s -> Printf.sprintf "%s.Internal.write %s" s (id s)) |> String.concat " ");
    if server then
      emit ~loc:__LINE__ "let request = request%d Internal.write %s"
        (List.length response)
        (response |> List.map ~f:(fun s -> Printf.sprintf "%s.Internal.read %s" s (id s)) |> String.concat " ");
  end;
  decr indent;
  emit "end";
  ()


let emit_class { Class.name; content; index; methods; doc } =
  (* Reorder modules based on dependencies *)
  let rec reorder methods =
    let rec move_down = function
      | { Method.response; _} as m :: x :: xs when
          List.exists ~f:(fun r -> List.exists ~f:(fun {Method.name; _} -> name = r) (x :: xs)) response -> x :: move_down (m :: xs)
      | x :: xs -> x :: move_down xs
      | [] -> []
    in
    let ms = move_down methods in
    if ms = methods then ms
    else reorder ms
  in
  let methods = reorder methods in
  emit_doc doc;
  emit ~loc:__LINE__ "module %s = struct" (variant_name name);
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

  List.iter ~f:(emit_method index) methods;

  decr indent;
  emit "end";
  ()

let emit_printer tree =
  emit_loc __LINE__;
  emit "module Printer = struct";
  incr indent;
  emit "let id_to_string (cid, mid) =";
  incr indent;
  emit "match cid with";
  incr indent;
  List.iter ~f:(function
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

let emit_specification tree =
  emit_loc __LINE__;
  emit "open Amqp_client_lib";
  emit "open Types";
  emit "open Protocol";
  emit "open Protocol_helpers";
  emit_domains tree
  |> List.iter ~f:(function Class x -> emit_class x | _ -> ());
  (* emit_printer tree; *)
  ()

type output = Constants | Specification

let () =
  (* Argument parsing *)
  let output_type = ref Specification in
  let filename = ref "" in

  Arg.parse
    ["-type", Arg.Symbol (["constants"; "specification"],
                          fun t -> output_type := match t with
                            | "constants" -> Constants
                            | "specification" -> Specification
                            | _ -> failwith "Illegal argument"
                         ), "Type of output";
     "-noloc", Arg.Clear emit_location, "Inhibit emission of location pointers"
    ]
    (fun f -> filename := f)
    "Generate protocol code";

  let xml =
    let in_ch = open_in !filename in
    let (_, xml) = Ezxmlm.from_channel in_ch in
    close_in in_ch;
    xml
  in
  let tree = xml |> parse_amqp in
  emit "(** Internal - Low level protocol description *)";
  emit "(***********************************)";
  emit "(* AUTOGENERATED FILE: DO NOT EDIT *)";
  emit "(* %s %s %s %s *)" Sys.argv.(0) Sys.argv.(1) Sys.argv.(2) Sys.argv.(3);
  emit "(***********************************)";
  emit "";
  emit "";

  begin
    match !output_type with
    | Constants ->
      emit_constants tree;
      ()
    | Specification -> emit_specification tree
  end;
  assert (!indent = 0);
  ()
