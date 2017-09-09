(**/**)
type 'a t = 'a option

let get ~default = function
  | None -> default
  | Some v -> v

let get_exn ?(exn=Invalid_argument "None") = function
  | None -> raise exn
  | Some v -> v

let map_default ~default ~f = function
  | None -> default
  | Some v -> f v

let map ~f = function
  | None -> None
  | Some v -> Some (f v)

let iter ~f = function
  | None -> ()
  | Some v -> f v
(**/**)
