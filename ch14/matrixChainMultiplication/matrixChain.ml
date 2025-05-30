open Base

(*let matrix_chain_order p n =
  let m = Array.make_matrix ~dimx:(n + 1) ~dimy:(n + 1) 0 in
  let s = Array.make_matrix ~dimx:(n + 1) ~dimy:(n + 1) 0 in
  let compute_min idx _ =
    let q = m.(idx).(j) + n.(k+1).(j) + p.(idx-1) * p.(k) * p.(j) in
    if idx >= i && idx <= j-1 then
      if q < m.(i).(j) then m.(i).(j) <- q; s.(i).(j) <- k else () 
    else
      () in
  Array.iteri m ~f:(fun idx ->
                      if idx >=k && idx <= (j - 1) then
                        compute_idx
                      else
                        ())

let matrix_chain_order p n =
  let m = Array.make_matrix ~dimx:(n + 1) ~dimy:(n + 1) 0 in
  let s = Array.make_matrix ~dimx:(n + 1) ~dimy:(n + 1) 0 in
  let group_lst = List.init (n-1) ~f:(fun t -> t + 2) in
  let compute_one_group i j =
    Array.mapi t~f:(fun k t ->
                    m.(i).(k) + m.(k+1).j + p.(i-1) * p.(k) * p.(j), k)
  List.iter group_lst ~f:compute_group_cost*)

let print_int_list lst = List.iter ~f:(Caml.Printf.printf "%d ") lst; Caml.Printf.printf "\n"

let print_int_pair_array arr =
  Array.iter ~f:(fun (x, y) -> Caml.Printf.printf "(%d, %d) " x y) arr;
  Caml.Printf.printf "\n"

let print_int_array_array title arr =
  Caml.Printf.printf "\n=====================%s======================\n" title;
  Array.iter ~f:(fun sub_arr ->
    Array.iter ~f:(fun x -> Caml.Printf.printf "%d\t" x) sub_arr;
    Caml.Printf.printf "\n"
  ) arr;
  Caml.Printf.printf "\n=============================================\n"

let matrix_chain_order p n =
  let m = Array.make_matrix ~dimx:(n + 2) ~dimy:(n + 2) 0 in
  let s = Array.make_matrix ~dimx:(n + 1) ~dimy:(n + 1) 0 in
  let cost_array i j pa =
    (*let _ = Caml.Printf.printf "cost_array(i:%d, j:%d)\n" i j in*)
    let tmp_cost =
      Array.init (j - i)
        ~f:(fun idx ->
            (*let _ = Caml.Printf.printf "idx:%d\n" idx in*)
            let k = i + idx in
              m.(i).(k) + m.(k+1).(j) +
              p.(i - 1) * p.(k) * p.(j), k) in
    (*let _ = print_int_pair_array tmp_cost in*)
    let q, k = Array.fold tmp_cost
          ~init:(Int.max_value, 0)
          ~f:(fun acc t ->
                let cost, idx = t in
                if cost < (fst acc) then t
                else acc) in
    m.(i).(j) <- q; s.(i).(j) <- k in
  let compute_chain l =
    (*let _ = Caml.Printf.printf "compute_chain\n" in*)
    let iter_lst =
      List.init (n - l + 1)
        ~f:(fun t -> t + 1) in
    List.iter iter_lst
      ~f:(fun i ->
            let j = i + l - 1 in
            m.(i).(j) <- Int.max_value;
            cost_array i j p
            ) in
  let idx_lst = List.init (n - 1) ~f:(fun t -> t + 2) in
  (*let _ = print_int_list idx_lst in*)
  List.iter idx_lst
      ~f:(fun l -> compute_chain l); m, s



let rec recursive_matrix_chain p i j m =
  let n = j - i in
  if n = 0 then 0
  else
  (*let _ = Caml.Printf.printf "i:%d, j:%d\n" i j in
  let _ = (!m).(i).(j) <- Int.max_value in*)
  let iter_lst = List.init n ~f:(fun idx -> idx + i) in
  let q_list = List.map iter_lst
                  ~f:(fun k ->
                        recursive_matrix_chain p i k m +
                        recursive_matrix_chain p (k+1) j m +
                        p.(i-1) * p.(k) * p.(j)
                      ) in
 (*let _ = Caml.Printf.printf "q_list:"; print_int_list q_list; Caml.Printf.printf "\n" in*)
  let q = List.fold_left q_list ~init:Int.max_value ~f:(fun acc t -> if acc < t then acc else t) in
  (!m).(i).(j) <- q;q

let recursive_matrix_chain_entry p i j =
  let n = j in
  let m = ref (Array.make_matrix ~dimx:(n + 2) ~dimy:(n + 2) 0) in
  recursive_matrix_chain p i j m


let p = [| 30; 35; 15; 5; 10; 20; 25|]

let rec print_optimal_parens s i j =
  if i = j then
    Printf.sprintf "A[%d]" i
  else
    let first = print_optimal_parens s i s.(i).(j) in
    let second = print_optimal_parens s (s.(i).(j) + 1) j in
    Printf.sprintf "(%s%s)" first second

let rec lookup_chain m p i j =
  if m.(i).(j) < Int.max_value then
    m.(i).(j)
  else
    if i = j then
      (m.(i).(j) <- 0;0)
    else
      let iter_lst = List.init (j - i - 1) ~f:(fun idx -> idx + i) in
      let q_list = List.map iter_lst
                  ~f:(fun k ->
                      lookup_chain m p i k +
                      lookup_chain m p (k + 1) j + p.(i-1) * p.(k) * p.(j)) in
      let q = List.fold_left q_list ~init:Int.max_value ~f:(fun acc t -> if acc < t then acc else t) in
      let _ = if q < m.(i).(j) then m.(i).(j) <- q else () in
      m.(i).(j)

let memoized_matrix_chain p n =
  let m = Array.make_matrix ~dimx:(n+1) ~dimy:(n+1) Int.max_value in
  lookup_chain m p 1 n

let () =
  let m, s = matrix_chain_order p 6 in
  let par_string = print_optimal_parens s 1 6 in
  (*print_int_array_array "m" m;
  print_int_array_array "s" s;*)
  Caml.Printf.printf "%s\n" par_string;
  let rmc = recursive_matrix_chain_entry p 2 4 in
  Caml.Printf.printf "%d\n" rmc;
  Caml.Printf.printf "%d\n" m.(2).(4);
  Caml.Printf.printf "%d\n" (lookup_chain m p 2 4)

