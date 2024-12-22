module MyHashTable =
  struct
    type 'a elem = {key : int; value : 'a}
    let table = Array.make 100 []
    let h t =  t / 100
    let chainedHashInsert x = table.(h(x.key)) <- x::table.(h(x.key))
    let chainedHashSearch k = List.find (fun t -> t.key = k) table.(h(k))
    let chainedHashDelete x = table.(h(x.key)) <- (List.filter (fun t -> t.key != x.key) table.(h(x.key)))
  end