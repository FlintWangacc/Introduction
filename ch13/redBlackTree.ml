type color = Red | Black

type ('a, 'b) rbtree = Leaf | Node of color * 'a * ' b * ('a, ' b) rbtree * ('a, 'b) rbtree

let root : ('a, 'b) rbtree ref = ref Leaf

let is_empty () = !root = Leaf

let rotate_right = function
    | Node (hc, key, value, x, rs) ->
      (
        match x with
          |Node (lc, lkey, lvalue, lls, lrs) -> Node (hc, lkey, lvalue, lls,
                                              Node (Red, key, value, lrs, rs)  (* new h *)
                                              )
          | _ -> failwith "Empty left node"
      )
    | _ -> failwith "Empty h node"

let rotate_left = function
    | Node (hc, key, value, ls, x) ->
      (
        match x with
        | Node (rc, rkey, rvalue, rls, rrs) -> Node (hc, rkey, rvalue, 
                                                Node (Red, key, value, ls, rls),
                                                rrs)
        | _ -> failwith "Empty right node"
      )
    | _ -> failwith "Empty h node"

let flip_colors = function
    | Node (hc, hkey, hvalue,
            Node(hlc, hlkey, hlvalue, hll, hlr), 
            Node(hrc, hrkey, hrvalue, hrl, hrr)) ->
      let flip = function
      | Red -> Black
      | Black -> Red in
      Node (flip hc, hkey, hvalue,
          Node (flip hlc, hlkey, hlvalue, hll, hlr),
          Node (flip hrc, hrkey, hrvalue, hrl , hrr))
    | _ -> failwith "Illegal Node"

let is_red = function
    | Node (Red, _, _ , _, _) -> true
    | Node (Black, _, _, _,_) | Leaf -> false

let fixup h =
  let fix1 h = match h with
    | Node (hc, _, _, hl, hr) ->
      (
        if is_red hr && (not (is_red hl))  then
          rotate_left h
        else
          h
      ) 
    | _ -> failwith "fix1 failed" in
  let fix2 h = match h with
    | Node (hc, _, _, Node (hlc, _, _, hll, hlr), _) ->
      (
        if hlc = Red && (is_red hll) then
          rotate_right h
        else
          h
      )
    | _ -> failwith "fix2 failed" in
  let fix3 h = match h with
    | Node (_, _, _, hl, hr) ->
      (
        if is_red hl && is_red hr then
          flip_colors h
        else
          h
      )
    | _ -> failwith "fix3 failed" in
    let h1 = fix1 h in
    let h2 = fix2 h1 in
    fix3 h2
  
let put (k : 'a) (v : 'b) : unit =
  let color_root_black = function
    | Node (_, key, value, left, right) -> Node (Black, key, value, left, right)
    | Leaf -> Leaf in
  let rec put_aux h k v =
    match h with
    | Leaf -> Node (Red, k, v, Leaf, Leaf)
    | Node (nc, nk, nv, l, r) ->
      (
        let new_node =
          if k < nk then
            Node (nc, nk, nv, put_aux l k v, r)
          else if k > nk then
            Node (nc, nk, nv, l, put_aux r k v)
          else
            Node (nc, nk, v, l, r) in
        fixup new_node
      )
  in
  let node = put_aux !root k v in
  let new_root = color_root_black node in
  root := new_root

let get key =
  let rec get_aux key h = 
  match h with
  | Node (hc, hk, hv, hl, hr) ->
      if hk = key then
        Some hv
      else if key < hk then
        get_aux key hl
      else
        get_aux key hr
  | Leaf -> None in
  match (get_aux key !(root)) with
  | None -> failwith "argument to get() is null"
  | Some v -> v
  
let move_red_left h =
  let flip_h = flip_colors h in
  match flip_h with
  | Node (hc, hkey, hvalue, hl, hr)->
      (
        match hr with
        | Node (hlc, hlkey, hlvalue, hrl, hrr) ->
            (
            if is_red hrl then
              let h' = Node(hc, hkey, hvalue, hl, rotate_right hr) in
              let h'' = rotate_left h' in
              let h''' = flip_colors h'' in
              h'''
            else
              flip_h
            )
        | _ -> failwith "h shouldn't be leaf"
      )
  | _ -> failwith "h shouldn't be leaf"

(*let balance = function
  | Node (hc, hkey, hvalue, hl, hr) as h -> (
      let h' = (if is_red hr && not (is_red hl) then
                        rotate_left h else h) in
      let h'' = (match h' with
      | Node (h'c, h'key, _,
                Node (h'lc, h'lkey, h'lvalue,
                      h'll, h'lr),
                _) ->
            if h'lc = Red && is_red h'll then
              rotate_right h'
            else
              h'
        | _ -> failwith "balance error 1"
      ) in
      match h'' with
      | Node (h''c, h''key, _, h''l, h''r) ->
        if is_red h''l && is_red h''r then
          flip_colors h''
        else
          h''
      | Leaf -> failwith "balance error 2"
  )
  | Leaf -> failwith "balance exception"*)

let balance = function
  | Node (hc, hk, hv,
          Node (Red, _, _, _, _),
          Node (Black, _, _,_, _)) as h -> rotate_left h
  | Node (hc, hk, hv,
          Node (Red, _, _,
            Node (Red, _, _, _, _), _),
          _) as h -> rotate_right h
  | Node (_, _, _,
          Node (Red, _, _, _, _),
          Node (Red, _, _, _, _)) as h -> flip_colors h
  | Leaf -> failwith "balance leaf not possible"
  | h -> h

(*let rec delete_min = function
  | Node (hc, hkey, hvalue, hl, hr) as h->
    (
      match hl with
      | Leaf -> Leaf
      | Node (hlc, hlkey, hlvalue, hll, hlr) ->(
        let h' =
          if not (is_red hl) && not (is_red hll) then
            let _ = print_string "test" in move_red_left h
          else
            h
          in
        let h'' = 
          match h' with
          | Node (h'c, h'key, h'value, h'l, h'r) ->
            delete_min h'l
          | Leaf -> Leaf 
        in
        balance(h'')
      )
    )
  | Leaf -> failwith "BST underflow"*)

let get_left = function
  | Node (_, _, _, left, _) -> left
  | Leaf -> failwith "Leaf has no left"

let gn = ref 1

let dump_dot tree (key_to_string: 'a -> string) (val_to_string : 'b -> string) =
  let link_dot value key_to_string = function
    | Leaf -> ""
    | Node (_, nkey, nval, _, _) -> Printf.sprintf " %s -> %s;\n"  (key_to_string value)  (key_to_string nkey)
  in
  let rec dump_aux node key_to_string id =
    match node with
    | Leaf -> ""
    | Node (color, key, value, left, right) ->
        let current_id = (key_to_string key) in
        let color_str = match color with Red -> "red" | Black -> "black" in
        let font_color_str = match color with Red -> "black" | Black -> "white" in
        let left_id = id + 1 in
        let right_id = id + 2 in
        let left_dot = dump_aux left key_to_string left_id in
        let right_dot = dump_aux right key_to_string right_id in
        let node_dot = Printf.sprintf " %s [label=\"(%s, %s)\" style=filled fillcolor=%s fontcolor=%s];\n" current_id (key_to_string key) (val_to_string value) color_str font_color_str in
        let edge_dot = (link_dot key key_to_string left)^
                      (link_dot key key_to_string right) in
          (*let edge_dot = Printf.sprintf " %s -> %s;\n" current_id (string_of_int left_id) ^
                         Printf.sprintf " %s -> %s;\n" current_id (string_of_int right_id) in*)
          node_dot ^ edge_dot ^ left_dot ^ right_dot
    in
    "digraph rbtee {\n" ^ dump_aux tree key_to_string 0 ^ "}\n"

let dump_string rbtree filename key_to_string val_to_string =
  let dot_output = dump_dot  (rbtree) (key_to_string) (val_to_string) in
  let oc = open_out filename in
  output_string oc dot_output

let rec delete_min_aux = 
  let delete_helper h =
    let h' = move_red_left h in
    let h'' = match h' with
              | Node (hc, hk, hv, hl, hr) -> Node (hc, hk, hv, delete_min_aux hl, hr)
              | Leaf -> failwith "can't be leaf" in
    balance h'' in
  function
  | Node (_, _, _, Leaf, _) -> Leaf
  | Node (_, _, _,
        Node (Black, _, _,
          Node(Black, _, _, _, _), _), _) as h ->
         delete_helper h
  | Node (_, _, _,
        Node (Black, _,_, Leaf, _),
         _) as h ->
          delete_helper h
  | Node (hc, hk, hv, hl, hr) -> Node (hc, hk, hv, delete_min_aux hl, hr) |> balance
  | Leaf -> failwith "delete_min_aux not touch Leaf"

let delete_min () =
  match !root with
  | Leaf -> failwith "BST underflow"
  | Node (hc, hkey, hvalue, hl, hr) ->
    (
      root := Node (Red, hkey, hvalue, hl, hr);
      root := delete_min_aux !root;
      match !root with
      | Node (hc, hkey, hvalue, hl, hr) -> root := Node (Black, hkey, hvalue, hl, hr)
      | _ -> root := Leaf
    )

let move_red_right h =
  let h' = flip_colors h in
  match h' with
  | Node (_, _, _, Node(_, _, _, Node(Red, _, _, _, _),
          _),_
        )  ->
    let h'' = rotate_right h' in
    flip_colors h''
  | h -> h

let rec delete_max_aux = 
  let delete_helper h =
    let h' = move_red_right h in
    let h'' = match h' with
              | Node (hc, hk, hv, hl, hr) -> Node (hc, hk, hv, hl, delete_max_aux hr)
              | Leaf -> failwith "can't be leaf" in
    balance h'' in
  function

  | Node (_, _, _,
          Node(Red, _, _, _, _), _) as h ->
      let h' = rotate_right h in
      (match h' with
      | Node (_, _, _, _, Leaf) -> Leaf
      | Node (_, _, _,
              _,
              Node(Red, _, _, Node(Red, _, _, _, _), _)) -> delete_helper h' |> balance
      | Node (hc, hk, hv, hl, hr) -> Node (hc, hk, hv, hl, delete_max_aux hr)
      | _ -> failwith "illegal case for delete_max_aux")
  | Node (_, _, _, _, Leaf) -> Leaf
  | Node (_, _, _,
          _,
          Node (Black, _, _,
            Node(Black, _,_, _, _), _)) as h ->
            delete_helper h
  | Node (_, _, _,
          _,
          Node (Black, _, _,
            Leaf, _)) as h ->
            delete_helper h
  | Node (hc, hk, hv, hl, hr) -> Node (hc, hk, hv, hl, delete_max_aux hr) |> balance
  | Leaf -> failwith "delete_max_au not torch Leaf"

let delete_max () =
  match !root with
  | Leaf -> failwith "BST underflow"
  | Node (hc, hkey, hvalue, hl, hr) ->
    (
      root := Node (Red, hkey, hvalue, hl, hr);
      root := delete_max_aux !root;
      match !root with
      | Node (hc, hkey, hvalue, hl, hr) -> root := Node (Black, hkey, hvalue, hl, hr)
      | _ -> root := Leaf
    )

(*let rec delete_aux h key compare =
  match h with
  | Node (hc, hk, hv, hl, hr) ->
      if compare key hk < 0 then
        let h' = match hl with Node (Black, _, _, Node(Black, _, _, _, _), _) -> move_red_left h | _ -> h in
        match h' with
          Node (hc, hk, hv, hl, hr) -> Node (hc, hk, hv, delete_aux hl key compare, hr) |> balance
        | _ -> failwith "delete_aux error 2"
      else
        let h' = if is_red hl then rotate_right h else h in
        (
          match h' with
            Node (hc, hk, hv, hl, hr) ->
              begin
                if (compare key hk = 0) && hr = Leaf then
                  Leaf
                else
                  let h'' = (match hr with
                  | Node (hrc, hrk, hrv, hrl, hrr) ->
                      if ((is_red h') |> not ) && ((is_red hrl) |> not) then
                        move_red_right h'
                      else
                        h'
                  | Leaf -> "delete_aux error 3")
              end
          | Leaf -> failwith "not implemented"
        )
  | _ -> failwith "delete_aux error"*)

let get_left = function
  | Node (hc, hk, hv, hl, hr) -> hl
  | Leaf -> failwith "get_left"

let is_leaf = function
  | Leaf -> true
  | _ -> false

let get_key = function
  | Node (hc, hk, hv, hl, hr) -> hk
  | Leaf -> failwith "get_key"

let get_right = function
  | Node (hc, hk, hv, hl, hr) -> hr
  | Leaf -> failwith "get_right"


let get_value = function
  | Node (_, _, value, _, _) -> value
  | Leaf -> failwith "leaf has no value"


let rec min_node = function
  | Node (hc, hk, hv, Leaf, hr) as h -> h
  | Node (hc, hk, hv, hl, hr) -> min_node hl
  | _ -> failwith "min_node"

let rotate_right_nece = function
  | Node (_, _, _,
        Node (Red, _, _, _, _), _) as h -> rotate_right h
  | Node _ as h -> h
  | Leaf -> failwith "rotate_right nece 1"


let rec delete_aux node key =
  match node with
  | Node (hc, hk, hv, hl, hr) as h ->(
      if key < hk then
        let h' = match hl with Node (Red, _, _, hl, hr) when is_red hl |> not -> move_red_left h | _ -> h in
        match h' with Node (hc, hk, hv, hl, hr) -> Node (hc, hk, hv, delete_aux hl key, hr) |_ -> failwith ""
      else
        let h' = if is_red hl then rotate_right h else h in
        if key = hk && is_leaf hr then
          let _ = print_endline "leaf" in
          Leaf
        else
          let h'' = match hr with
          | Node (Black, _, _, hl, hr) -> if is_red hl |> not then move_red_right h' else h'
          | _ -> h' in
          match h'' with
          | Node (hc, hk, hv, hl, hr) when key = hk ->
                                        let x = min_node hr in
                                        Node (hc, get_key x, get_value x, hl, delete_min_aux hr)
          | Node (hc, hk, hv, hl, hr) -> Node (hc, hk, hv, hl, delete_aux hr key)
          | _ -> failwith ""
          )
                                           
  | _ -> failwith ""

let delete key  =
  (match !root with
  | Node (hc, hkey, hvalue, hl, hr) ->
    if (is_red hl|> not) && (is_red hr |> not) then
      root := Node (Red, hkey, hvalue, hl, hr)
    else
      ()
  | _ -> ());
  root := delete_aux !root key;
  (match !root with
  | Node (hc, hkey, hvalue, hl, hr) -> root := Node (Black, hkey, hvalue, hl, hr)
  | _ -> root := Leaf)

let rec move_red_left_test v = function
  | Node (hc, hv, hk, hl, hr) as h ->
    if v = hv then
      move_red_left h
    else if v < hv then
      Node (hc, hv, hk,
            move_red_left_test v hl,
            hr)
    else
      Node (hc, hv, hk,
            hl,
            move_red_left_test v hr)
  | _ -> failwith "Not exist"

(*let _ =
  let lower = List.init 26 (fun i -> char_of_int (int_of_char 'a' + i)) in
  let upper = List.init 26 (fun i -> char_of_int (int_of_char 'A' + i)) in
  let lst = lower @ upper in
  List.iter (fun c -> put c (int_of_char c)) lst;
  dump_string !root "before.dot";
  delete 'p';
  dump_string !root "after.dot"*)

(*let () =
  let lst = List.init 3 (fun i -> char_of_int (int_of_char 'a' + i)) in
  List.iter (fun c -> put c (int_of_char c)) lst;
  dump_string !root "before.dot";
  delete_min ();
  dump_string !root "after.dot"*)