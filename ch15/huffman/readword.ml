open Stdio

let is_punctuation = function
  | ',' | '.' | '!' | '?' | ';' | ':' | '(' | ')' | '[' | ']' | '{' | '}' | '\'' | '"' | '-' | '#' -> true
  | _ -> false

let read_words filename  =
  let inch = In_channel.create filename in
  let rec read_loop acc =
    match In_channel.input_char inch with
    | Some c ->
        if c = ' ' || c = '\t' || c = '\n' then
          read_loop (String.make 1 c :: acc)  (* Add whitespace as a word *)
        else if is_punctuation c then
          let punct = String.make 1 c in
          read_loop (punct :: acc)
        else
          let word = Buffer.create 16 in
          Buffer.add_char word c;
          read_word_loop word acc
    | None -> List.rev acc  (* Return words in correct order *)
  
  and read_word_loop word acc =
    match In_channel.input_char inch with
    | Some c when c <> ' ' && c <> '\t' && c <> '\n' && not (is_punctuation c) ->
        Buffer.add_char word c;
        read_word_loop word acc
    | Some c ->  (* Hit whitespace *)
        let word_str = Buffer.contents word in
        let whitespace_str = String.make 1 c in
        read_loop (whitespace_str :: word_str :: acc)
    | None ->  (* EOF *)
        let word_str = Buffer.contents word in
        List.rev (word_str :: acc)
  
  in
  read_loop []

let write_strings_to_file filename strings =
  let oc = open_out filename in
  List.iter (fun s -> output_string oc (s)) strings;
  close_out oc
(*let construct_word_map words =
  List.fold_left
    (fun map word ->
      let count =
        try StringMap.find word map with Not_found -> 0
      in
      StringMap.add word (count + 1) map)
    StringMap.empty
    words*)

(*let () =
  let words = read_words "dickens.txt" in
  List.iter (fun word ->
    if word = " " then print_string "[SPACE] "
    else if word = "\t" then print_string "[TAB] "
    else if word = "\n" then print_string "[NEWLINE] "
    else print_string (word ^ " ")
  ) words*)
