let rec merge l1 l2 = match (l1, l2) with
  | (h1::t1, h2::t2) -> if h1 < h2 then h1::merge t1 (h2::t2)
                        else h2::merge (h1::t1) t2
  | ([], []) -> []
  | (l1, []) -> l1
  | ([], l2) -> l2

let rec mergeSort l =
  let split lst = let half = (List.length lst) / 2 in
        (List.filteri
        (fun idx a -> if idx < half then true else false) lst,
        List.filteri 
        (fun idx a -> if idx >= half then true else false) lst) in
  if List.length l = 1 then
    l
  else
    let left, right = split l in  merge (mergeSort left) (mergeSort right)

let () = List.iter (fun t -> print_int t; print_string " ")
          (mergeSort [821;874;784323;8273;3827;378297;8927;28973;34;8732;873;23]); print_newline()