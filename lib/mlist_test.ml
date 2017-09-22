let test_empty =
  let t = Mlist.create () in
  assert ((Mlist.take ~pred:(fun _ -> true) t) = None);
  ()

let test_take =
  let t = Mlist.create () in
  Mlist.append t 1;
  assert ((Mlist.take ~pred:(fun _ -> true) t) = (Some 1));
  Mlist.append t 1;
  Mlist.append t 2;
  Mlist.append t 3;
  Mlist.append t 4;
  assert ((Mlist.take ~pred:(fun _ -> true) t) = (Some 1));
  assert ((Mlist.take ~pred:(fun _ -> true) t) = (Some 2));
  assert ((Mlist.take ~pred:(fun _ -> true) t) = (Some 3));
  assert ((Mlist.take ~pred:(fun _ -> true) t) = (Some 4));
  assert ((Mlist.take ~pred:(fun _ -> true) t) = None);
  ()

let test_while =
  let t = Mlist.create () in
  Mlist.append t 1;
  Mlist.append t 2;
  Mlist.append t 3;
  Mlist.append t 4;

  assert (Mlist.take_while ~pred:(fun n -> n < 4) t = [1;2;3]);
  ()
