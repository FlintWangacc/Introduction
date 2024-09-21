let _ = Printexc.record_backtrace true


let swap arr i j =
  (*let _ = print_newline (); Array.iter (Printf.printf "%d\t") arr; print_newline() in *)
  let tmp = arr.(i) in
  arr.(i) <- arr.(j); arr.(j) <- tmp

let (/-) a b = (a + b - 1) / b

let partitionAround arr p r x =
  let g = (r - p + 1) / 5 in
  let xidx = (p + 2 * g) +  (g /- 2) in
  let _ = swap arr xidx r in
  let li = ref p and si = ref (r - 1) in
    while !li < !si do
      while arr.(!li) <= x && !li <= r - 1 do
        incr li
      done;
      while arr.(!si) > x && !si >= p do
        decr si
      done;
      if !li < !si then
        swap arr !li !si
      else
        ()
    done;
    swap arr !li r;
  !li

let randomList n max_val =
  (*let _ = Random.self_init () in*)
  let lst = List.init n (fun _ -> Random.int max_val) in
  (lst, Array.of_list lst)
    
let lst, arr = randomList 1128 71677

let tlst = List.sort (Int.compare) lst
    

let rec select arr p r i =
  let _ = assert (i <=  r - p + 1) in
  let j = ref p in
  let retVal = ref (-1) in
  let is = ref i  in
  while (r - !j + 1) mod 5 != 0 do
    (*let _ = Printf.printf "\nis:%d\n" !is in*)
    for idx = (!j) to (r) do
      if arr.(!j) > arr.(idx) then
        swap arr !j idx
      else
        ()
    done;
    if !is = 0 then
      retVal := arr.(!j)
    else
      ();
    is := !is - 1;
    j := !j + 1
  done;
  if !retVal != -1 then
    !retVal
  else
    let g = (r - !j + 1) / 5 in
    for idx = !j to (!j + g - 1) do   (* Sort each group *)
      for pi = 1 to 4 do
        let ins = idx + g * pi  in
          let rins = ref ins in
          while !rins > idx && arr.(!rins) < arr.(!rins - g) do
            swap arr (!rins) (!rins - g);
            rins := !rins - g
          done
      done
    done;
    let _ = assert (g!=0) in
    let x = select arr (!j + 2 * g) (!j + 3 * g) (g /- 2) in
    let q = partitionAround arr !j r x in
    let k = q - !j + 1 in
    if !is = k - 1 then
      arr.(q)
    else if !is < k then
      select arr !j (q-1) (!is)
    else
      select arr (q+1) r (!is-k)

(*let _ = Printf.printf "\nlist:\n"; List.iteri (fun idx t -> Printf.printf "%d:%d\t"  idx t) tlst*)
let t1 = select arr 0 1127 556 


let _ = Printf.printf "\n%d, %d\n" (List.nth tlst 556) t1