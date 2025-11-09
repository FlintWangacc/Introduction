open Set
let s = [|Int.min_int; 1; 3; 0; 5; 3; 5;  6;  7;  8;  2; 12|]
let f = [|0;           4; 5; 6; 7; 9; 9; 10; 11; 12; 14; 16|]

(*let int_set = Set.empty (module Int)*)

(*module IntPairMap = Map.Make(
  struct
    type t = int
    let compare = compare
  end
  )

let event_map = IntPairMap.of_list
  [(0, (Int.min_int, 0));
   (1, (1, 3));
   (2, (3, 5));
   (3, (0, 6));
   (4, (5, 7));
   (5, (3, 9));
   (6, (5, 9));
   (7, (6, 10));
   (8, (7, 11));
   (9, (8, 12));
   (10, (2, 14));
   (11, (12, 16))]*)

module IntSet = Set.Make(Int)

let dump_set set =
  Printf.printf "Set contents: {";
  IntSet.iter (fun x -> Printf.printf "%d " x) set;
  Printf.printf "}\n"

let activity_selector s f k n =
  let rec recursive_activity_selector s f k n p =
    let retSet = IntSet.empty in
    let m = k + 1 in
    (*let _ = Printf.printf "m:%d\n" m in*)
    if m > n then
      IntSet.empty
    else
      (*let _ = Printf.printf "s.(m):%d, f.(k):%d\n" s.(m) f.(k) in*)
      if s.(m) >= f.(p) then
        let remain_set =
            recursive_activity_selector s f m n m in
        (*let _ = Printf.printf "%d\n" m in*)
        IntSet.add m retSet |> IntSet.union remain_set
      else
        if m <= n then
          recursive_activity_selector s f m n p
        else
          IntSet.empty
  in
  recursive_activity_selector s f k n 0


