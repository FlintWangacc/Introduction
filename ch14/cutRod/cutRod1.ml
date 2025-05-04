let price = [|0; 1; 5; 8; 9; 10; 17; 17; 20; 24; 30|]
let price2 = [1; 5; 8; 9; 10; 17; 17; 20; 24; 30]

let rec cut_rod p n =
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
              let sub_p = List.sub p 0 n in
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
