(**/**)
type 'a elem = { content: 'a; mutable next: 'a cell }
and 'a cell = Nil
            | Cons of 'a elem

type 'a t = { mutable first: 'a elem;
              mutable last: 'a elem }

(** create an empty list *)
let create () =
  let sentinal = { content = Obj.magic (); next = Nil } in
  { first = sentinal;
    last = sentinal; }

(** Returns the first element that satisfies [pred] and removes it from the list. O(n) *)
let take ~pred t =
  let rec inner = function
    | Nil -> None
    | Cons ({ content = _; next = Cons { content; next} } as cell) when pred content ->
        cell.next <- next;
        begin match next with Nil -> t.last <- cell | _ -> () end;
        Some content;
    | Cons { content = _; next } ->
        inner next
  in
  inner (Cons t.first)

(** Peek at the first element without removing it from the list. O(1) *)
let peek t =
  match t.first.next with
  | Nil -> None
  | Cons { content; _ } -> Some content

(** Pop the first element from the list. O(1) *)
let pop t =
  match t.first.next with
  | Nil -> None
  | Cons { content; next } ->
      t.first.next <- next;
      if (next = Nil) then t.last <- t.first else ();
      Some content

(** Removes and returns elements while statisfying [pred]. O(m), where m is number of elements returned *)
let take_while ~pred t =
  let rec inner = function
    | Nil -> []
    | Cons ({content = _; next = Cons { content; next } } as cell) when pred content ->
        cell.next <- next;
        if (next = Nil) then t.last <- cell else ();
        content :: inner (Cons cell);
    | Cons _ -> []
  in
  inner (Cons t.first)

(** Prepends an element to the list *)
let prepend t v =
  let e = { content=v; next = t.first.next } in
  t.first.next <- Cons e;
  if (e.next = Nil) then t.last <- e else ()

(** Appends a element to the list *)
let append t v =
  let e = { content=v; next = t.last.next } in
  t.last.next <- Cons e;
  t.last <- e

(**/**)
