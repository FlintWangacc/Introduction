let _ = Printexc.record_backtrace true

(*let debugArray arr =
  let al = Array.length arr in
  if al < 5 then
    Array.iter (fun t -> Printf.printf "%d\t" t) arr
  else
    let rem = al mod 5 in
    let sub1 = Array.sub arr 0 rem in
    let sub2 = Array.sub arr (rem) (al - rem) in
    Array.iter (fun t -> Printf.printf "%d\t" t) sub1;
    print_newline ();
    Array.iteri (fun idx t ->
                    Printf.printf "%d\t" t;
                    if (idx + 1) mod 5 = 0 then
                      Printf.printf "\n"
                    else
                      ()) sub2;
  print_newline ()*)

let debugArrayRange arr p r title =
  let rec dumpList num s = function
    | hd :: tl ->  if num mod 5 = 0 then print_newline () else (); Printf.printf "%d:%d\t" s hd; dumpList (num-1) (s+1) tl
    | _ -> print_newline () in
  let _ = Printf.printf "%s:\n" title in
  let subLst = Array.sub arr p (r-p+1) |> Array.to_list in
  dumpList (r - p + 1) p subLst

(*let debugArrayRange arr2 p r title =
  (*let _ = debugArray arr2 in*)
  let _ = Printf.printf "%s:\n" title in
  let arr = Array.sub arr2 p (r-p+1) in
  let al = Array.length arr2 in
  if al < 5 then
    Array.iter (fun t -> Printf.printf "%d\t" t) arr
  else if (r-p+1) mod 5 != 0 then
    let rem = al mod 5 in
    let sub1 = Array.sub arr 0 rem in
    let sub2 = Array.sub arr (rem) (al - rem) in
    if Array.length sub1 != 0 then
      Array.iter (fun t -> Printf.printf "%d\t" t) sub1
    else
      ();
    print_newline ();
    Array.iteri (fun idx t ->
                    Printf.printf "%d\t" t;
                    if (idx + 1) mod 5 = 0 then
                      Printf.printf "\n"
                    else
                      ()) sub2
  else
    Array.iteri (fun idx t ->
      Printf.printf "%d\t" t;
      if (idx + 1) mod 5 = 0 then
        Printf.printf "\n"
      else
        ()) arr2;
  print_newline ()*)


let swap arr i j =
  (*let _ = print_newline (); Array.iter (Printf.printf "%d\t") arr; print_newline() in *)
  let tmp = arr.(i) in
  arr.(i) <- arr.(j); arr.(j) <- tmp

let (/-) a b = (a + b - 1) / b

let partitionAround arr p r x rlevel =
  let _ = Printf.printf "\npartitionAround argument: p(%d), r(%d) rlevel(%d)\n" p r rlevel in
  let _ = debugArrayRange arr p r "arr:" in
  let _ = rlevel in
  let g = (r - p + 1) / 5 in
  let xidx = (p + 2 * g) +  (g /- 2) in
  let _ = Printf.printf "partitionAround debug: g(%d), xidx(%d), arr.(xidx):%d, x:%d\n" g xidx arr.(xidx) x in
  let _ = assert (arr.(xidx) = x) in
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

  
 (* let partitionAround arr p r =
    let xidx = 0 in
    let x = arr.(xidx) in
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
    !li*)


let randomList n max_val =
  (*let _ = Random.self_init () in*)
  let lst = List.init n (fun _ -> Random.int max_val) in
  (lst, Array.of_list lst)
    
let lst, arr = randomList 1128 71677

let tlst = List.sort (Int.compare) lst
    

let rec select arr p r i rlevel =
  (*let _ = Unix.sleep 1 in*)
  let _ = Printf.printf "\n[%d]select argument: p:%d, r:%d, i:%d\n" rlevel p r i in
  let _ = assert (i <=  r - p + 1) in
  let _ = debugArrayRange arr p r "arr:" in
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
  (*let _ = Printf.printf "After first section is:%d j:%d\n" !is !j in*)
  if !retVal != -1 then
    let _ = Printf.printf "[%d]select(p:%d, r:%d, i:%d) return:%d\n" rlevel p r i !retVal in
    !retVal
  else
    let g = (r - !j + 1) / 5 in
    (*let _ = Printf.printf "g:%d\n" g in*)
    for idx = !j to (!j + g - 1) do   (* Sort each group *)
      for pi = 1 to 4 do
        let ins = idx + g * pi  in
        (*let _ = Printf.printf "ins:%d, idx:%d, arr.(ins):%d, arr.(idx):%d\n" ins idx arr.(ins) arr.(idx) in*)

          let rins = ref ins in
          (*let _ = Printf.printf "rins:%d\tvalue:%d\n" !rins arr.(!rins) in*)
          while !rins > idx && arr.(!rins) < arr.(!rins - g) do
            
            swap arr (!rins) (!rins - g);
            rins := !rins - g
          done
      done
    done;
    let _ = Printf.printf "After sort:\n" in
    let _ = debugArrayRange arr !j r "arr:" in
    let _ = assert (g!=0) in
    (*let _ = Printf.printf "Middle select(j:%d, g:%d)\n" !j g in*)
    let x = select arr (!j + 2 * g) (!j + 3 * g) (g /- 2) (rlevel + 1) in
    (*let _ = assert (x = List.nth tlst (!j + 2 * g + g /- 2)) in*)
    let q = partitionAround arr !j r x rlevel in
    (*let _ = Printf.printf "partitionAround return:q(%d), x:%d, rlevel:%d\n" q x rlevel in*)
    let k = q - !j + 1 in
    let _ = Printf.printf "\nis:%d, k:%d\n" !is k in
    let _ = Printf.printf "debug k(%d), is(%d)\n" k !is in 
    if !is = k - 1 then
      let _ = Printf.printf "[%d]select2(p:%d, r:%d, i:%d) return:%d\n" rlevel p r i arr.(q) in
      arr.(q)
    else if !is < k then
      (*let _ = Printf.printf "!!!!Debug:%d(j)\n" !j in*)
      select arr !j (q-1) (!is) (rlevel + 1)
    else
      select arr (q+1) r (!is-k) (rlevel + 1)

(*let selectMod5 lst p r i =

let rec select arr p r i =
  if (r - p + 1) mod 5 != 0 then*)


let _ = Printf.printf "\nlist:\n"; List.iteri (fun idx t -> Printf.printf "%d:%d\t"  idx t) tlst
let t1 = select arr 0 1127 556 0


let _ = Printf.printf "\n%d, %d\n" (List.nth tlst 556) t1

(*let _ = Printf.printf "%d" (select (snd (randomList 88 299)) 0 87 86);*)