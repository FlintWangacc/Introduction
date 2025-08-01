open Core

module StringIntMap = Map.Make(String)

let count_words word_list =
  let update_count map word =
    let current_count = Map.find map word |> Option.value ~default:0 in
    Map.set map ~key:word ~data:(current_count + 1)
  in
  List.fold word_list ~init:(Map.empty (module String)) ~f:update_count

let map_to_sorted_list map =
  Map.to_alist map
    |> List.sort ~compare:(fun (_, v1) (_, v2) -> Int.descending v1 v2)
  

(*let () =
  let filename = "dickens.txt" in
  let words = Readword.read_words filename in
  let wordsMap = count_words words in
  Map.iteri wordsMap ~f:(fun ~key ~data ->
    printf "'%s' appears %d times\n" key data
  )*)