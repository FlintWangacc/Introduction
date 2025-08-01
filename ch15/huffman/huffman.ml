open Base

module Heap = CCHeap

type huffmanNode = {
  left : huffmanNode option;
  right: huffmanNode option;
  freq : int;
  word : string option;
}

module HuffmanNodeHeap = Heap.Make(struct
  type t = huffmanNode
  let leq a b = a.freq < b.freq
end)

let huffmanList lst =
  List.map lst
    ~f:(fun (str, freq) ->
          {left = None; right = None; freq = freq; word = Some str})

let wl = Readword.read_words "dickens.txt"  (* word list *)

let wsl = Wordlist.map_to_sorted_list (Wordlist.count_words wl)   (* word map list *)
let hheap = ref (wsl |> huffmanList |> HuffmanNodeHeap.of_list)   (* word list reference *)
(*let huffman *)

let extract_min q =
  match HuffmanNodeHeap.take (!q) with
  | Some (nheap, min) -> q := nheap; min
  | None -> failwith "take from empty queue"

let insert q z =
  q := HuffmanNodeHeap.add (!q) z

let huffuman (c : HuffmanNodeHeap.t) =
  let n = HuffmanNodeHeap.size c in
  let q = ref c in
  let create_node _ =
    let x = extract_min q in
    let y = extract_min q in
    let z = {left = Some x; right = Some y; freq = x.freq + y.freq; word = None} in
    insert q z in
  for i = 1 to (n - 1) do
    (*print_int i; print_newline ();*)
    create_node i
  done;
  extract_min q

let huffumanStr (n : huffmanNode) lstr =
  (*let lstr = String.fold ~init:[] ~f:(fun acc c -> c :: acc) str |> List.rev in*)
  let rec huffumanStrAux (n : huffmanNode) = function
    | '0' :: rest as remain ->
      (
        match n with
        | {left = Some left; right; freq ; word = None } -> huffumanStrAux left rest
        | {left; right; freq; word = Some rstr } -> rstr, remain 
        | _ -> failwith "illegal"
      )
    | '1' :: rest as remain ->
      begin
        match n with
        | {left; right = Some right; freq; word = None} -> huffumanStrAux right rest
        | {left; right; freq; word = Some rstr } -> rstr, remain
        | _ -> failwith "illegal"
      end
    | [] ->
      begin
        match n with
        | {left; right; freq; word = Some rstr} -> rstr, []
        | _ -> failwith "illegal last word" 
      end
    | _ -> failwith "it should be 0 or 1"
  in
  huffumanStrAux n lstr

let bstr2words  (n : huffmanNode) bstr =
  let lbstr = String.fold ~init:[] ~f:(fun acc c -> c :: acc) bstr |> List.rev in
  let rec bstr2words_aux n blst acc =
    match (huffumanStr n blst) with
    | word, [] -> List.rev (word::acc)
    | word, lst -> bstr2words_aux n lst (word :: acc)
  in
  bstr2words_aux n lbstr []



module StringMap = Core.Map.Make(String)

let build_word_map root =
  let rec traverse node path acc =
    match node with
    | { word = Some w; freq; left = None; right = None } ->
      (match Map.add ~key:w ~data:(String.concat ~sep:"" (List.rev path), freq) acc with
      | `Ok new_map -> new_map
      | `Duplicate -> acc
      )
    | { left = Some l; right = Some r; _ } ->
      let acc' = traverse l ("0" :: path) acc in
      traverse r ("1" :: path) acc'
    | { left = Some l; right = None; _} ->
      traverse l ("0" :: path) acc
    | { left = None; right = Some r; _ } ->
      traverse r ("1" :: path) acc
    | _ -> acc
    in
    traverse root [] StringMap.empty

let ht = huffuman !hheap

let wm = build_word_map ht

let wsl2bsl wsl =
  let getBsl word =
    match Map.find wm word with
    | Some (str, _) -> str
    | None -> failwith "not found" in
  List.map wsl ~f:getBsl

(*let bsl = wsl2bsl wl*)


let find_node_by_code root code_str =
  let rec traverse node = function
    | [] -> node  (* Reached the end of the code string *)
    | '0' :: rest -> 
        (match node.left with
         | Some left_node -> traverse left_node rest
         | None -> failwith "illegal string")
    | '1' :: rest ->
        (match node.right with
         | Some right_node -> traverse right_node rest
         | None -> failwith "illegal string")
    | _ -> failwith "illegal"  (* Invalid character in code *)
  in
  let code_list =
    String.to_list code_str
  in
  match traverse root code_list with
  | { word = Some _; _ } as node -> Some node  (* Only return nodes with words *)
  | _ -> None

let huffman_decode lst =
  List.map lst
    ~f:(fun str ->
          match find_node_by_code ht str with
          | Some {left = _; right = _; freq =_; word = Some w} -> w
          | _ -> failwith "illegal node")

(*let bit_strings = String.concat ~sep:"" bsl*)




let write_bytes_to_file filename bytes =
  let oc = open_out_bin filename in
  List.iter ~f:(output_byte oc) bytes;
  close_out oc



let int_to_binary n =
  if n < 0 || n > 255 then
    invalid_arg "Input must be between 0 and 255"
  else
    let rec helper n count acc =
      if count = 0 then acc
      else helper (n lsr 1) (count - 1) (string_of_int (n land 1) ^ acc)
  in
  helper n 8 ""

let string_to_bit_list s =
  let rec str2bit_aux i acc =
    if i < 0 then acc
    else str2bit_aux (i - 1) (s.[i] :: acc)
  in
  str2bit_aux (String.length s - 1) [] |> List.map ~f:(fun t -> match t with '0' -> 0 | '1' -> 1 | _ -> failwith "illegal")

(*let bit_list = string_to_bit_list bit_strings*)

let int64_to_bytes_be (n : Int64.t) : int list = 
  List.init 8 ~f:(fun i ->
    Int64.to_int_exn (Int64.bit_and (Int64.shift_right n ((7 - i) * 8)) 0xFFL))

let bytes_to_int64_be (bytes : int list) : Int64.t =
  if List.length bytes <> 8 then
    failwith "Input must be exactly 8 bytes";
  List.foldi bytes ~init:0L ~f:(fun i acc byte ->
    Int64.bit_or acc (Int64.shift_left (Int64.of_int byte) ((7 - i) * 8)))

let bits_to_bytes bits =
  let rec bits_to_bytes_aux acc byte idx = function
    | bit :: rest -> let new_byte = (byte lsl 1) lor bit in
                     if idx = 7 then
                        bits_to_bytes_aux (new_byte :: acc) 0 0 rest
                     else
                        bits_to_bytes_aux acc new_byte (idx+1) rest
    | [] -> if idx = 0 then List.rev acc
            else List.rev (byte::acc)
  in
  let header = int64_to_bytes_be (Int64.of_int (List.length bits)) |> List.rev in
  bits_to_bytes_aux header 0 0 bits

(*let bytes_list = bits_to_bytes bit_list*)

let write_bytes_to_file filename bytes =
  let oc = open_out_bin filename in
  List.iter ~f:(output_byte oc) bytes;
  close_out oc

(*let _ = write_bytes_to_file "dickens.huf" bytes_list*)

let read_bytes_from_file filename =
  let ic = open_in_bin filename in
  let rec read_all_bytes acc =
    try
      let byte = input_byte ic in
      read_all_bytes (byte :: acc)
    with End_of_file ->
      close_in ic;
      List.rev acc
  in
  read_all_bytes []

(*let read_bytes_list = read_bytes_from_file "dickens.huf"

let read_bits_string = let longstr = List.map read_bytes_list ~f:int_to_binary  |> String.concat ~sep:"" in
                        String.sub longstr 0 (bit_length)

let recover_words = bstr2words ht read_bits_string*)

let rec node_to_json node =
  `Assoc [
    ("freq", `Int node.freq);
    ("word", match node.word with
      | None -> `Null
      | Some w -> `String w);
    ("left", match node.left with
      | None -> `Null
      | Some n -> node_to_json n);
    ("right", match node.right with
      | None -> `Null
      | Some n -> node_to_json n)
  ]

let save_tree_json filename tree =
  let oc = open_out filename in
  try
    Yojson.Basic.to_channel oc (node_to_json tree);
    close_out oc
  with e ->
    close_out oc;
    raise e

(*let rec json_to_node = function
  | `Assoc [
    ("freq", `Int freq);
    ("word", word_json);
    ("left", left_json);
    ("right", right_json)
  ] ->
    {
      freq;
      word = (match word_json with
        | `Null -> None
        | `String w -> Some w);
      left = (match left_json with
        | `Null -> None
        | j -> Some (json_to_node j));
      right = (match right_json with
        | `Null -> None
        | j -> Some (json_to_node j))
    }
  | _ -> failwith "Invalid JSON format"*)

let rec json_to_node json =
  match json with
  | `Assoc fields ->
    let find_field name =
      try Stdlib.List.assoc name fields
      with Not_found -> `Null
    in
    let freq = match find_field "freq" with
      | `Int n -> n
      | _ -> failwith "Invalid frequency field"
    in
    let word = match find_field "word" with
      | `Null -> None
      | `String s -> Some s
      | _ -> failwith "Invalid word field"
    in
    let left = match find_field "left" with
      | `Null -> None
      | j -> Some (json_to_node j)
    in
    let right = match find_field "right" with
      | `Null -> None
      | j -> Some (json_to_node j)
    in
    { left; right; freq; word}
  | _ -> failwith "Expected JSON object"


let load_tree_json filename =
  let ic = open_in filename in
  try
    let json = Yojson.Basic.from_channel ic in
    close_in ic;
    json_to_node json
  with e ->
    close_in_noerr ic;
    raise e

let pack_files file1 file2 output_file =
  let zip = Zip.open_out output_file in
  let add_file filename =
    let contents =
      let ic = open_in_bin filename in
      let len = in_channel_length ic in
      let buf = Bytes.create len in
      really_input ic buf 0 len;
      close_in ic;
      Bytes.to_string buf
    in
    Zip.add_entry contents zip filename
  in
  add_file file1;
  add_file file2;
  Zip.close_out zip

let unzip_to_tmp zip_file =
  let zip = Zip.open_in zip_file in
  let tmp_dir = "/tmp" in
  (try Unix.mkdir tmp_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  List.iter ~f:(fun entry ->
    let output_path = Filename.concat tmp_dir entry.Zip.filename in
    if not entry.Zip.is_directory then (
      let dir = Filename.dirname output_path in
      (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
      let contents = Zip.read_entry zip entry in
      let oc = open_out_bin output_path in
      output_string oc contents;
      close_out oc
    )) (Zip.entries zip);
    Zip.close_in zip;
    List.map ~f:(fun entry ->
      let tmp_dir = "/tmp" in
      Filename.concat tmp_dir entry.Zip.filename) (Zip.entries zip)

let change_suffix filename new_suffix =
  let base =
    try Filename.chop_extension filename
    with Invalid_argument _ -> filename
  in
  if String.compare new_suffix "" = 0 then base
  else base ^ "." ^ new_suffix


let zip_file filename = 
  let wl = Readword.read_words filename in
  let wsl = Wordlist.map_to_sorted_list (Wordlist.count_words wl) in
  let hheap = ref (wsl |> huffmanList |> HuffmanNodeHeap.of_list) in
  let ht = huffuman !hheap in
  let bsl = wsl2bsl wl in
  let bit_strings = String.concat ~sep:"" bsl in
  let bit_list = string_to_bit_list bit_strings in
  let bytes_list = bits_to_bytes bit_list in
  let data_file = change_suffix filename "data" in
  let _ = write_bytes_to_file data_file bytes_list in
  let _ = save_tree_json "huffTree.json" ht in
  let output = change_suffix filename "compress" in
  let _ = pack_files "huffTree.json" data_file output in
  let _ = Unix.unlink "huffTree.json" in
  let _ = Unix.unlink data_file in
  output

let unzip_file filename outfile =
  let file_lst = unzip_to_tmp filename in
  let huffman_tree =
    match List.find ~f:(fun filename ->
                      let regex = Str.regexp ".*\\.json$" in Str.string_match regex filename 0) file_lst with
    | Some json_file -> load_tree_json json_file
    | _ -> failwith "no json file" in 
  let words_list =
    match List.find ~f:(fun filename ->
      let regex = Str.regexp ".*\\.data$" in Str.string_match regex filename 0) file_lst with
    | Some data_file -> let bytes_list = read_bytes_from_file data_file  in
                        let bit_length_list = List.sub ~pos:0 ~len:8 bytes_list in
                        let _ = List.iter bit_length_list ~f:(fun t -> Stdlib.Printf.printf "%d, " t);  print_newline () in
                        let bit_length = bytes_to_int64_be bit_length_list |> Int64.to_int_exn in
                        let data_num = List.length bytes_list - 8 in
                        let data_list = List.sub ~pos:8 ~len:data_num bytes_list in
                        let _ = Stdlib.Printf.printf "bit_length:%d\n" bit_length in
                        String.sub (List.map  ~f:int_to_binary data_list |> String.concat ~sep:"") 0 bit_length
                        |> bstr2words huffman_tree
    | _ -> failwith "no data file" in
  let oc = Out_channel.open_text outfile in
  List.iter ~f:(fun str -> Out_channel.output_string oc str) words_list;
  Out_channel.close oc

let usage () =
  Stdlib.Printf.eprintf "Usage:\n";
  Stdlib.Printf.eprintf "  To zip: %s -z <file-to-zip>\n" Sys.argv.(0);
  Stdlib.Printf.eprintf "  To unzip: %s -u <zip-file> <output-file>\n" Sys.argv.(0);
  ignore(exit 1)

let main () =
  if Array.length Sys.argv < 2 then usage ();
  match Sys.argv.(1) with
  | "-z" | "--zip" ->
    if Array.length Sys.argv < 3 then usage ();
    let file_to_zip = Sys.argv.(2) in
    ignore(zip_file file_to_zip)
  | "-u" | "--unzip" ->
    if Array.length Sys.argv < 4 then usage ();
    let zip_file = Sys.argv.(2) in
    let output_file = Sys.argv.(3) in
    unzip_file zip_file output_file

  | _ -> usage ()

let () = main ()