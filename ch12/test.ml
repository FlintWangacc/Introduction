let () = Printexc.record_backtrace true

let init_tree () =
  let _ = Random.init 1234 in
  let uniq_cons x xs = if List.mem x xs then xs else x::xs in
  let remove_from_right xs = List.fold_right uniq_cons xs [] in
  let min, max = 1, 200 in
  let random_list = List.init 20 (fun _ -> Random.int_in_range ~min ~max) in
  let random_list = remove_from_right random_list in
  let tree_root = Bst.make_root() in
  let list_node = List.map (fun t -> Bst.make_node t) random_list  in
  List.iter (fun t -> Bst.tree_insert tree_root t) list_node;
  let str = Bst.preorder_traverse (!tree_root) (fun a -> a) in
  Bst.export_to_dot !tree_root (fun a -> a) "test.dot";
  print_string str;
  tree_root

let remove_root bst =
  let ret_node = Bst.tree_search bst 92 in
  (*let ret_node2 = Bst.tree_successor (Option.get ret_node) in*)
  let _ = Bst.tree_delete (bst) (Option.get ret_node) in
  (*Printf.printf "%d\n" (Bst.get_key (Option.get ret_node2));*)
  Bst.export_to_dot bst (fun a -> a) "test2.dot"
(*let main () =
  let tree_root = Bst.make_root () in
  let new_node = Bst.make_node 5 in
  let node3 = Bst.make_node 3 in
  let node7 = Bst.make_node 7 in
  let node1 = Bst.make_node 1 in
  let node4 = Bst.make_node 4 in
  let _ = Bst.tree_insert  (tree_root) new_node in
  let _ = Bst.tree_insert (tree_root) (node3) in
  let _ = Bst.tree_insert (tree_root) (node7) in
  let _ = Bst.tree_insert (tree_root) (node1) in
  let _ = Bst.tree_insert (tree_root) (node4) in
  let str = Bst.preorder_traverse (!tree_root) (fun a -> a) in
  export_to_dot (((!tree_root).root.contents |> Option.get)) "test.dot";
  print_string str*)

let main () =
  let bst = init_tree () in
  remove_root !bst 
let _ = main ()