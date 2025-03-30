type color = Red | Black

type 'a rbtree = Leaf | Node of color * 'a * 'a rbtree * 'a rbtree

(*
        Case 1

          Bz
        /   \
      Ry     d
     /  \
  [Rx]   c
  /  \
 a    b
*)

(*
      Case 2

        Bz
       /  \
      Rx   d
    /   \
   a    [Ry]
       /   \
      b     c
*)

(*
    Case 3

      Bx
     /  \
    a    Rz
        /  \
      [Ry]  d
     /   \
    b     c
*)

(*
    Case 4

      Bx
     /  \
    a    Ry
        /  \
       b   [Rz]
          /   \
         c     d
*)

(*
  Target
     Ry
    /  \
   Bx  Bz
  / \  / \
 a   b c  d
*)
let balance = function
  | Black, z, Node (Red, y, Node (Red, x, a, b), c), d
  | Black, z, Node (Red, x, a, Node (Red, y, b, c)), d
  | Black, x, a, Node (Red, z, Node (Red, y, b, c), d)
  | Black, x, a, Node (Red, y, b, Node (Red, z, c, d)) ->
    Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
  | a, b, c, d -> Node(a, b, c, d)

let make_root key = Node (Black, key, Leaf, Leaf)

let rb_insert x s =
  let rec ins = function
    | Leaf -> Node (Red, x, Leaf, Leaf)
    | Node (color, y, a, b) as s ->
      if x < y then balance (color, y, ins a, b)
      else if x > y then balance (color, y, a, ins b)
      else s
  in
  match ins s with
  | Node (_, y, a, b) -> Node (Black, y, a, b)
  | Leaf ->
    failwith "RBT insert failed with ins returning leaf"

let dump_dot tree (to_int: 'a -> int) =
  let link_dot value to_int = function
    | Leaf -> ""
    | Node (_, nval, _, _) -> Printf.sprintf " %d -> %d;\n" (to_int value) (to_int nval)
  in
  let rec dump_aux node to_int id =
    match node with
    | Leaf -> ""
    | Node (color, value, left, right) ->
        let current_id = (to_int value) in
        let color_str = match color with Red -> "red" | Black -> "black" in
        let font_color_str = match color with Red -> "black" | Black -> "white" in
        let left_id = id + 1 in
        let right_id = id + 2 in
        let left_dot = dump_aux left to_int left_id in
        let right_dot = dump_aux right to_int right_id in
        let node_dot = Printf.sprintf " %d [label=\"%d\" style=filled fillcolor=%s fontcolor=%s];\n" current_id (to_int value) color_str font_color_str in
        let edge_dot = (link_dot value to_int left)^
                      (link_dot value to_int right) in
        (*let edge_dot = Printf.sprintf " %s -> %s;\n" current_id (string_of_int left_id) ^
                       Printf.sprintf " %s -> %s;\n" current_id (string_of_int right_id) in*)
        node_dot ^ edge_dot ^ left_dot ^ right_dot
    in
    "digraph rbtee {\n" ^ dump_aux tree to_int 0 ^ "}\n"

let rotate_right = function
    | Node (hc, key, x, rs) ->
      (
        match x with
          |Node (lc, lkey, lls, lrs) -> Node (hc, lkey , lls,
                                              Node (Red, key, lrs, rs)  (* new h *)
                                              )
          | _ -> failwith "Empty left node"
      )
    | _ -> failwith "Empty h node"

let rotate_left = function
    | Node (hc, key, ls, x) ->
      (
        match x with
        | Node (rc, rkey, rls, rrs) -> Node (hc, rkey,
                                                Node (Red, key, ls, rls),
                                                rrs)
        | _ -> failwith "Empty right node"
      )
    | _ -> failwith "Empty h node"

let flip_colors = function
    | Node (hc, hkey,
            Node(hlc, hlkey, hll, hlr), 
            Node(hrc, hrkey, hrl, hrr)) ->
      let flip = function
      | Red -> Black
      | Black -> Red in
      Node (flip hc, hkey,
          Node (flip hlc, hlkey, hll, hlr),
          Node (flip hrc, hrkey, hrl , hrr))
    | _ -> failwith "Illegal Node"

let example_tree =
  Node (Black, 10,
    Node (Red, 5, Leaf, Leaf),
    Node (Red, 15, Leaf, Leaf))

let random_int_list length max_value =
  let rec aux n acc =
    if n <= 0 then acc
    else
      let rand_num = Random.int max_value in
      aux (n - 1) (rand_num :: acc)
  in
  aux length []

let rec is_black_height_balanced tree =
  let rec black_height_aux node black_count =
    match node with
    | Leaf -> Some (black_count + 1)    (* properties 3: Every leaf (NIL) is black *)
    | Node (color, _, left, right) ->
        let left_height = black_height_aux left (black_count + (if color = Black then 1 else 0)) in
        let right_height = black_height_aux right (black_count + (if color = Black then 1 else 0)) in
        match left_height, right_height with
        | Some lh, Some rh when lh = rh -> Some lh
        | _ -> None
  in
  black_height_aux tree 0

let rec is_valid_rbtree tree =
  let rec node_aux node =
    match node with
    | Leaf -> true
    | Node (color, _, left, right) ->
        let is_red = (color = Red) in
        let left_valid = node_aux left in
        let right_valid = node_aux right in
        let no_double_red = match left, right with (* Properties 4 *)
          | Node (Red, _, _, _), _ -> not is_red
          | _, Node (Red, _, _, _) -> not is_red
          | _ -> true
        in
        left_valid && right_valid && no_double_red
  in
  match tree with
  | Leaf -> true
  | Node (Black, _, _, _) -> node_aux tree
  | _ -> false (* properties 2: The root is black*)
and is_valid_rbtree_helper tree =
  let height_check = is_black_height_balanced tree (* Properties 5*) in
  is_valid_rbtree tree && height_check <> None

let init_tree () =
  Random.self_init ();
  let length = 50 in
  let max_value = 1000 in
  let random_list = random_int_list length max_value in
  List.fold_right rb_insert random_list Leaf

let () =
  let rbtree = init_tree () in
  let _ = Printf.printf "%b\n" (is_valid_rbtree rbtree) in
  let dot_output = dump_dot  (rbtree) (fun a -> a) in
  let oc = open_out "rbtree1.dot" in
  output_string oc dot_output;
  let rbtree2 = flip_colors rbtree in
  let oc2 = open_out "rbtree2.dot" in
  let dot_output2 = dump_dot (rbtree2) (fun a -> a) in
  output_string oc2 dot_output2;
  close_out oc;
  close_out oc2