let partition arr =
  let x, tla = (List.rev arr |> List.hd), (arr |> List.rev |> List.tl |> List.rev) in
  let nl_less = List.filter (fun t -> t <= x) tla in
  let nl_larger = List.filter (fun t -> t > x) tla in
  nl_less, x, nl_larger

let rec quicksort arr =
  (*let _ = List.iter (Printf.printf "%d\t") arr; Printf.printf "\n" in*)
  if List.length arr > 1 then
    let (a1, q, a2) = partition arr in
    let na1 = quicksort a1 in
    let na2 = quicksort a2 in
    na1 @ (q::na2)
  else
    arr

let _ = quicksort [2; 8; 7; 1; 4; 3; 5; 6; 4] |> List.iter (Printf.printf "%d\t"); print_newline ()
