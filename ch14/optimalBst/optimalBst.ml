open Base

let dump_float_array arr =
  Array.iter ~f:(fun inner_array ->
    Array.iter ~f:(fun value ->
      Caml.Printf.printf "%.2f " value
    ) inner_array;
    Caml.Printf.printf "\n"
  ) arr

let dump_int_array arr =
  Array.iter ~f:(fun inner_array ->
    Array.iter ~f:(fun value ->
      Caml.Printf.printf "%d " value
    ) inner_array;
    Caml.Printf.printf "\n"
  ) arr

let dump_int_list lst =
  List.iter ~f:(fun value ->
    Caml.Printf.printf "%d " value
  ) lst;
  Caml.Printf.printf "\n"

let optimal_bst p q n =
  let e = Array.make_matrix ~dimx:(n+2) ~dimy:(n+1) 0. in
  let w = Array.make_matrix ~dimx:(n+2) ~dimy:(n+1) 0. in
  let root = Array.make_matrix ~dimx:(n+1) ~dimy:(n+1) 0 in
  let init_array idx sub_arr =
    if idx >= 1 then
      sub_arr.(idx-1) <- q.(idx-1)
    else () in
  let _ = Array.iteri e ~f:init_array in
  (*let _ = Caml.Printf.printf "e:\n"; dump_float_array e in*)
  let _ = Array.iteri w ~f:init_array in
  let compute_min_range i j =
    (*let _ = Caml.Printf.printf "i:%d, j:%d\n" i j in*)
    let _ = e.(i).(j) <- Float.max_value in
    let _ = w.(i).(j) <- w.(i).(j-1) +. p.(j) +. q.(j) in
    let r_lst = List.init (j - i + 1) ~f:(fun idx -> i + idx) in
    let e_lst = List.map r_lst ~f:(fun r -> e.(i).(r-1) +. e.(r+1).(j) +. w.(i).(j)) in
    let t, r = List.foldi e_lst ~init:(Float.max_value, 0) 
              ~f:(fun idx (ts, rs) t ->
                    if (Float.compare t ts) < 0 then
                      (t, i + idx)
                    else (ts, rs)) in
    (*let _ = Caml.Printf.printf "t:%f, r:%d\n" t r in*)
    e.(i).(j) <- t; root.(i).(j) <- r(*; Caml.Printf.printf "end\n"*) in
  let compute_fix_range l =
    (*let _ = e.(i).(j) <- Float.max_value in
    let _ = w.(i).(j) <- w.(i).(j-1) +. p.(j) +. q.(j) in*)
    let i_lst = List.init (n - l + 1) ~f:((+) 1) in
    (*let _ = Caml.Printf.printf "i list:" in
    let _ = dump_int_list i_lst in*)
    List.iter i_lst ~f:(fun i -> let j = i + l - 1 in
                              compute_min_range i j) in
  let l_lst = List.init n ~f:(fun idx -> 1 + idx) in
  (*let _ = Caml.Printf.printf "l:\n" in
  let _ = dump_int_list l_lst in*)
  let _ = List.iter l_lst ~f:(fun l -> compute_fix_range l) in
  e, root

let p = [|0.; 0.15; 0.10; 0.05; 0.10; 0.20|]
let q = [|0.05; 0.10; 0.05; 0.05; 0.05; 0.10|]

let () =
  let e, root = optimal_bst p q 5 in
  let _ = Caml.Printf.printf "e:\n"; dump_float_array e in
  let _ = Caml.Printf.printf "root:\n"; dump_int_array root in
  ()