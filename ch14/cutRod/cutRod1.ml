open Core
open Stdio
let price = [|0; 1; 5; 8; 9; 10; 17; 17; 20; 24; 30|]
let price2 = [0; 1; 5; 8; 9; 10; 17; 17; 20; 24; 30]

let cut_rod p n =
  if n = 0 then
       0
  else
    let nl = List.init n ~f:(fun x -> x) in
    let pl = List.map nl ~f:(fun t -> p.(t) + p.(n-t)) in
    List.fold_left ~f:(fun t p -> if p > t then p else t) ~init:Int.min_value pl

let rec memoized_cut_rod_aux p n r =
  match r.(n) = Int.min_value with
  | true -> (
      match n with
      | 0 -> 0
      | _  -> (
              let sub_p = List.sub p ~pos:0 ~len:n in
              let rl = List.mapi sub_p
                        ~f:(fun idx t -> 
                            if n-idx-1 > 0 then
                              t + memoized_cut_rod_aux p (n-idx-1) r
                            else
                              t) in
              (*let _ = List.iter rl ~f:(fun t -> Printf.sprintf "%d, " t |>
                                          print_string) in*)
              r.(n) <- 
              List.fold_left ~f:(fun t p -> if p > t then p else t)
                ~init:Int.min_value rl;
              r.(n))
  )
  | false -> r.(n)

let memoized_cut_rod p n =
  let r = Array.init (n + 1) ~f:(fun _ -> Int.min_value) in
  memoized_cut_rod_aux p n r

(*let extended_bottom_up_cut_rod p n j =
  let r = Array.init (n+1) ~f:(fun _ -> 0) in
  let p_sub = List.sub p 1 j in
  let q_t = List.mapi p_sub ~f:(fun i t ->
                                  if i = 0 then
                                    Int.min_value
                                  else
                                    (List.nth_exn p_sub j) + r.(j-i)
                                ) in
  r.(j) <- List.fold_left q_t ~init:Int.min_value ~f:(fun acc t -> if acc < t then t else acc)*)

(*let extended_bottom_up_cut_rod p n =
  let r = Array.init (n+1) ~f:(fun _ -> 0) in
  let s = Array.init (n+1) ~f:(fun _ -> 0) in
  let idx_arr = Array.init (n+1) ~f:(fun idx -> idx + 1) in
  Array.map2_exn
    r idx_arr
    (fun t1 j ->
      let q_arr = Array.sub ~pos:0 ~len:j p in
      let q1_arr = Array.mapi q_arr ~f:(fun i t -> t + r.(j-i)) in
      Array.foldi q1_arr
        ~init:0 ~f:(fun i acc t ->
                      if (t > acc) then
                        (s.(j) <- i; t)
                      else
                        acc)
    ), s

let print_cut_rod_solution p n =
  let r, s = extended_bottom_up_cut_rod p n in
  let _ = Array.iter r ~f:(Caml.Printf.printf "%d, ") in
  let _ = Array.iter s ~f:(Caml.Printf.printf "%d, ") in
  let t = ref n in
  r, s*)
  (*while !t > 0 do
    print_int s.(!t);
    print_newline ();
    (*Caml.Printf.printf "%d\n" !t;*)
    t := !t - s.(!t)
  done*)

(*let extended_bottom_up_cut_rod p n =
  let r = List.init (n+1) ~f:(fun _ -> 0) in
  let s = List.init (n) ~f:(fun _ -> 0) in
  let j = n in
  let sub_p = List.sub p ~pos:1 ~len:j in
  let subr_r = List.sub r ~pos:1 ~len:j |> List.rev in
  let tmp_sum = Caml.List.map2 (fun t1 t2 -> t1 + t2) sub_p subr_r  in
  Caml.List.mapi (fun idx t -> (idx, t)) tmp_sum*)

let print_array =
  Array.iter ~f:(Caml.Printf.printf "%d, ")

type 'a lazy_list = Nil | Cons of 'a * (unit -> 'a lazy_list)

let rec lazy_to_list = function
  | Nil -> []
  | Cons (hd, tl) -> hd :: (lazy_to_list (tl ()))

let extended_bottom_up_cut_rod p n =
  let r = Array.init (n+1) ~f:(fun _ -> 0) in
  let s = Array.init (n+1) ~f:(fun _ -> 0) in
  let max_price p r j =
    (*let _ = Caml.Printf.printf "\n%d:\n" j in*)
    let sub_p = Caml.Array.sub p 1 j in
    (*let _ = Caml.Printf.printf "Sub_p:\n"; print_array sub_p; Base.print_newline () in*)
    let sub_rev_r = Caml.Array.sub r 0 j |> Array.rev in
    (*let _ = Caml.Printf.printf "Sub_rev_r:\n"; print_array sub_rev_r; Base.print_newline () in*)
    (*let _ = Caml.Printf.printf "%d, %d\n" (Array.length sub_p) (Array.length sub_rev_r) in*)
    let sum_arr = Caml.Array.map2 (fun t1 t2 -> t1 + t2) sub_p sub_rev_r in
    (*let _ = Base.print_newline (); print_array sum_arr in*)
    let max_p = Array.foldi sum_arr ~init:Int.min_value ~f:(fun idx acc t -> if acc < t then (s.(j) <- idx+1; t) else acc) in
    r.(j) <- max_p in
  let idx_lst = List.init (n) ~f:(fun i -> i + 1) in
  let _ = List.map idx_lst ~f:(fun t -> max_price p r t) in
  r, s

let print_cut_rod_solution p n =
  let r, s = extended_bottom_up_cut_rod p n in
  let rstr = Printf.sprintf "%d: " r.(n) in
  (*let split_list = List.init 
  let nr = ref n in*)
  let rec split n =
    Cons (s.(n), fun () -> if ((n - s.(n))) > 1 then split (n - s.(n)) else Nil) in
  let split_list = lazy_to_list (split n) in
  List.fold_left ~init:rstr ~f:(fun acc t -> let st = string_of_int t in
                                              if String.compare acc rstr = 0 then acc ^ st 
                                              else acc ^ ", " ^ st) split_list
  (*while (!nr) > 1 do
    Caml.Printf.printf "%s:%d,\n" rstr s.(!nr) in
    nr := !nr - s.(!nr)
  done;*)

let () =
  print_string (print_cut_rod_solution price 9); print_endline ""
