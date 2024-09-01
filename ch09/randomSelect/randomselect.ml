

let dumpArray str arr =
  let _ = Printf.printf "%s:" str in
  Array.iter (Printf.printf "%d, ") arr; print_newline ()


let randomPartition arr p r =
  let _ = Printf.printf "p:%d, r:%d\n" p r in
  let _ = dumpArray "randomPartition:" arr in
  let (arr1, arr2, arr3) = (Array.sub arr 0 (p), Array.sub arr (p) (r-p), Array.sub arr (r+1) (Array.length arr - r - 1)) in
  let _ = dumpArray "arr1:" arr1; dumpArray "arr2:" arr2; dumpArray "arr3:" arr3 in
  let swap arr i j =
    let t = arr.(i) in
    arr.(i) <- arr.(j); arr.(j) <- t in 
  let num = r - p in
  let i = p + (Random.int num)  in
  (*let i = r - 1 in*)
  Printf.printf "here\n";
  swap arr i (r-1);
  let pivot = arr.(r) in
  let _ = Printf.printf "pivot:%d\n" pivot in
  let arrPair = Array.fold_left
    (fun (a1, a2) t ->
      if t < pivot then
        (Array.append a1 [|t|] , a2)
      else
        (a1, Array.append a2 [|t|])
    ) ([||], [||]) arr2 in
    dumpArray "fst:" (fst arrPair);
    dumpArray "snd:" (snd arrPair);
    let na = Array.concat [arr1; fst arrPair; [|pivot|]; snd arrPair; arr3]  in
    assert (Array.length na == Array.length arr); (na, Array.length (fst arrPair) + Array.length arr1)

let rec randomSelect arr p r i =
  if p = r then arr.(p)
  else 
    let newArr,q = randomPartition arr p r in
    let _ = dumpArray "arr:" arr; dumpArray "newArr:" newArr in
    let k = q - p + 1 in
    if i = k then
      newArr.(q)
    else
      let _ = Printf.printf "p:%d, q:%d, r:%d, i:%d, k:%d\n" p q r i k in
      if i < k then
        let _ = Printf.printf "first:" in randomSelect newArr p (q-1) i
      else
        let _ = Printf.printf "second:" in randomSelect newArr (q+1) (r) (i-k)

let t = [| -1; 6; 19; 4; 12; 14; 9; 15; 7; 8 ; 11;3; 13; 2; 5; 10|]
let sortT = [| -1; 6; 19; 4; 12; 14; 9; 15; 7; 8 ; 11;3; 13; 2; 5 ; 10|]
let _ = Array.sort (fun a b -> a - b) sortT

let test idx = randomSelect t 1 (Array.length t - 1) idx == sortT.(idx - 1)

let _ = Printf.printf "\n%d\n" (randomSelect t 1 14 3)