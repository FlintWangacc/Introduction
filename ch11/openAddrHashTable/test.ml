let main () =
  let hash v n =
    let h1 v = v mod 701 in
    let h2 v = v mod 700 in
    ((h1 v) + n * (h2 v)) mod 1000 in
  let hashTable = Openhashtbl.create 1000 hash in
  let _ = Openhashtbl.hashInsert hashTable 12345 in
  let t = Openhashtbl.hashSearch hashTable 12345 in
  print_int (Option.get t)

let _ = main ()