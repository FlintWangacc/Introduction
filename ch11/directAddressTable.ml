module DirectAddressTable =
  struct
    type 'a elem = {key:int; value : 'a}
    let table = Array.make 100 None 
    let directAddressSearch k = table.(k)
    let directAddressInsert x = table.(x.key) <- Some x
    let directAddressDelete x = table.(x.key) <- None
  end