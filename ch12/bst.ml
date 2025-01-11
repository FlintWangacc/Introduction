let () = Printexc.record_backtrace true

type 'a t =
{ left : 'a t option ref;
  key : 'a;
  right : 'a t option ref;
  p : 'a t option ref
}

type 'a bst = { root : 'a t option ref }

let make_root () = ref { root = ref None }

let make_node key =
  {left = ref None; key = key; right = ref None; p = ref None}

let rec inorder_tree_walk t dump =
  match t with
  | None -> ()
  | Some ts -> (
    inorder_tree_walk !(ts.left) dump;
    dump ts.key;
    inorder_tree_walk !(ts.right) dump
  )

let tree_search bst key =
  let rec search t key =
    match t with
    | None -> None
    | Some ts -> (
        if key < ts.key then
          search !(ts.left) key
        else if key > ts.key then
          search !(ts.right) key
        else
          Some ts
      ) in 
  search !(bst.root) key

let iterative_tree_search t key =
  let x = ref t in
  while (Option.is_some (!x)) &&  ((Option.get (!x)).key != key) do
    let xn = Option.get (!x) in
    if key < xn.key then
      x := !(xn.left)
    else
      x := !(xn.right)
  done;
  !x

let tree_minimum t =
  let x = ref t in
  while (Option.is_some !((Option. get (!x)).left)) do
    x := !((Option.get (!x)).left)
  done;
  !x

let tree_maximum t =
  let x = ref t in
  while (Option.is_some !((Option. get (!x)).right)) do
    x := !((Option.get (!x)).right)
  done;
  !x

let tree_successor t =
  if (Option.is_some !(t.right)) then
    tree_minimum !(t.right)
  else
    let x = ref t in
    let y = ref ((!x).p) in
    while (Option.is_some (!(!y))) && (!x) = (Option.get !((Option.get (!(!y))).right)) do
      x := (Option.get (!(!y)));
      y := (Option.get (!(!y))).p
    done;
    !(!y)

let tree_insert bst z =
  let x = ref (!bst.root) in
  let y = ref None in
  while (Option.is_some !(!x)) do
    y := !(!x);
    if z.key < (Option.get !(!x)).key then
      x := (Option.get !(!x)).left
    else
      x := (Option.get !(!x)).right
  done;
  z.p := !y;
  if Option.is_none !y then
    (!bst).root := Some z
  else if z.key < (Option.get !y).key then
    (Option.get !y).left := Some z
  else
    (Option.get !y).right := Some z

let transplant bst u v =
  if Option.is_none !((u).p) then
    bst.root := v
  else if u == (((!(u.p) |> Option.get).left).contents |> Option.get) then
    (!(u.p) |> Option.get).left := v
  else
    (!(u.p) |> Option.get).right := v;
  if Option.is_some v then
    (v |> Option.get).p := !(u.p)

let tree_delete bst z =
  if Option.is_none !(z.left) then
    transplant bst z !(z.right)
  else if Option.is_none !(z.right) then
    transplant bst z !(z.left)
  else
    let y = tree_minimum !(z.right) in
      if y != !(z.right) then begin
        let y' = Option.get y in
        transplant bst y' !(y'.right);
        (Option.get y).right := !(z.right);
        (Option.get !((Option.get y).right)).p := y
      end
      else
        begin
        ()
        end;
      transplant bst z y;
      (Option.get y).left := !(z.left);
      (Option.get !((Option.get y).left)).p := y

let rec to_dot node to_int =
  match node with
  | None -> ""
  | Some n ->
    let left_dot = to_dot !(n.left) to_int in
    let right_dot = to_dot !(n.right) to_int in
    let current_dot = Printf.sprintf "    \"%d\";\n"  (n.key |> to_int) in
    let connections =
      let left_connection =
        match !(n.left) with
        | None -> ""
        | Some left_node -> Printf.printf "   \"%d\" -> \"%d\";\n" (n.key |> to_int) (left_node.key |> to_int); Printf.sprintf  "   \"%d\" -> \"%d\";\n"  (n.key |> to_int) (left_node.key |> to_int) in
      let right_connection =
        match !(n.right) with
        | None -> ""
        | Some right_node -> Printf.printf "   \"%d\" -> \"%d\";\n" (n.key |> to_int) (right_node.key |> to_int); Printf.sprintf "   \"%d\" -> \"%d\";\n" (n.key |> to_int) (right_node.key |> to_int) in
      left_connection ^ right_connection
    in
    current_dot ^ connections ^ left_dot ^ right_dot

let rec preorder node to_int =
  match !node with
  | None -> ""
  | Some n -> 
    let current_node = Printf.sprintf " %d," (n.key |> to_int) in
    let left = preorder n.left to_int in
    let right = preorder n.right to_int in
    left ^ current_node ^ right

let preorder_traverse bst to_int =
  let root = bst.root in
  preorder root to_int

let export_to_dot bst to_int filename =
  let dot_content = "digraph BST {\n" ^ (to_dot !(bst.root) to_int) ^ "}\n" in
  let oc = open_out filename in
  output_string oc dot_content;
  close_out oc

let get_key node = node.key