let countingSort arr n k =
  let b = Array.init (n + 1) (fun _ -> 0) in
  let c = Array.init k (fun _ -> 0) in
  List.iter (fun t -> c.(t) <- c.(t) + 1) arr;
  Array.iteri (fun idx t -> if idx = 0 then 
                              c.(idx) <- t
                            else 
                              c.(idx) <- t + c.(idx-1))
              c;
  for j = n downto 1 do
    let t = List.nth arr (j-1) in
    b.(c.(t)) <- t;
    c.(t) <- c.(t) -1
  done;
  Array.to_list b |> List.tl

let _ = countingSort [2; 5; 3; 0; 2; 3; 0; 3] 8 6 |> List.iter (Printf.printf "%d\t"); print_newline ()