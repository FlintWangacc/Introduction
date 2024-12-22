let main () =
  let hash v n =
    let h1 v = v mod 701 in
    let h2 _ = 1 in
    ((h1 v) + n * (h2 v)) mod 1000 in
  let hashTable = LinearProbing.create 1000 hash in
  let _ = LinearProbing.hashInsert hashTable 12345 in
  let t = LinearProbing.hashSearch hashTable 12345 in
  print_int (Option.get t)

let _ = main ()