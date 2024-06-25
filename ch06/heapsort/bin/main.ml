(*open Random*)

type 'a heap = {arr: 'a array; heapSize : int ref}

(*let parent i = i / 2*)

let left i = 2 * i

let right i = 2 * i + 1

let swap arr i j =
  let t = arr.(i) in
  arr.(i) <- arr.(j); arr.(j) <- t

let rec maxHeapify heap i =
  let l = left i and r = right i in
  let getArray arr idx = if idx <= !(heap.heapSize) then (arr.(idx), idx) else (arr.(0), 0) in 
  let lst = [getArray heap.arr l; getArray heap.arr r] in
  let largest = List.fold_left (fun acc t -> if fst t > fst acc then t else acc) (heap.arr.(i), i) lst in
  if (snd largest != i) then
    let _ = swap heap.arr i (snd largest) in maxHeapify heap (snd largest)
  else
    ()

let build_max_heap heap n =
  heap.heapSize := n;
  for i = n/2 downto 1 do
    maxHeapify heap i
  done
  

let heapSort heap n =
  build_max_heap heap n;
  for i = n downto 2 do
    swap heap.arr 1 i;
    heap.heapSize := !(heap.heapSize) - 1;
    maxHeapify heap 1
  done

let () =
  let arr = Array.init 11 (fun i -> if i = 0 then 0 else Random.int 42) in
  let heap = {arr = arr; heapSize = ref 0} in
  heapSort heap 10;
  Array.iteri (fun i t -> if i < (Array.length heap.arr - 1) then Printf.printf "%d, " t else Printf.printf "%d\n" t) heap.arr
