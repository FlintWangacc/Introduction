let rec insertNewLst elem = function
  | h :: t -> (
              if h > elem then
                [h] @ insertNewLst elem t
              else
                [elem] @ [h] @ t
    )
  | [] -> [elem]

let insertionSort lst =
  let rec insertionSortAux rlst = function
  | h::t -> insertionSortAux (insertNewLst h rlst) t
  | [] -> rlst in
  List.rev (insertionSortAux [] lst)

let () = List.iter (fun t -> print_int t; print_string " ")
          (insertionSort [5; 2; 4; 6; 1; 3]); print_newline()