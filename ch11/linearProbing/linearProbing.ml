type 'a t =
{ mutable size: int;
  data : 'a elem array;
  hashfunc : 'a -> int -> int
} and 'a elem =
    Empty
  | Val of 'a

exception UnderfFlow of string

let create num hash =
  {size = 0; data = Array.make num Empty; hashfunc = hash }

let hashInsert t k =
  let ret = ref None in
  for i = 0 to t.size do
    let q = t.hashfunc k i in
    match t.data.(q) with
     | Empty -> ret := Some q; t.size <- t.size + 1; t.data.(q) <- Val k
     | Val _ -> ()
  done;
  match !ret with
  | None -> raise (UnderfFlow "hash table overflow")
  | Some q -> q

let hashSearch t k =
  let ret = ref None in
  for i = 0 to t.size do
    let q = t.hashfunc k i in
    match t.data.(q) with
    | Val v -> if v == k then ret := Some q else ()
    | Empty -> ()
  done;
  match !ret with
  | None -> None
  | Some q -> Some q