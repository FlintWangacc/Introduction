module RBTree = RedBlackTree

let _ =
  RBTree.(
  let lower = List.init 26 (fun i -> char_of_int (int_of_char 'a' + i)) in
  let upper = List.init 26 (fun i -> char_of_int (int_of_char 'A' + i)) in
  let lst = lower @ upper in
  let root = List.fold_left (fun root c -> put root c (int_of_char c)) Leaf lst in
  let _ = dump_string root "before.dot" (fun t -> String.make 1 t) string_of_int in
  let new_root = delete root 'p' in
  dump_string new_root "after.dot" (fun t -> String.make 1 t) string_of_int)