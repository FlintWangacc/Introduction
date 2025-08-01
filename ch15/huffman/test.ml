(* File: copy_file_split_words.ml *)

let () =
  (* Check command line arguments *)
  if Array.length Sys.argv <> 3 then
    Printf.eprintf "Usage: %s <input_file> <output_file>\n" Sys.argv.(0)
  else
    let input_file = Sys.argv.(1) in
    let output_file = Sys.argv.(2) in

    (* Open the input file for reading *)
    let in_channel = open_in input_file in
    (* Open the output file for writing *)
    let out_channel = open_out output_file in

    try
      (* Read and write the entire content character by character *)
      let rec copy_content () =
        let c = input_char in_channel in
        (* Check if the character is a whitespace character *)
        if c = ' ' || c = '\t' || c = '\n' then
          output_char out_channel c  (* Write whitespace characters directly *)
        else
          let rec read_word () =
            let next_char = input_char in_channel in
            if next_char = ' ' || next_char = '\t' || next_char = '\n' then (
              output_char out_channel next_char;  (* Write whitespace character *)
              ()
            ) else (
              output_char out_channel next_char;  (* Write the character of the word *)
              read_word ()  (* Continue reading the word *)
            )
          in
          output_char out_channel c;  (* Write the first character of the word *)
          read_word ()  (* Read the rest of the word *)
      in
      (* Copy all characters *)
      try
        while true do
          copy_content ()
        done
      with End_of_file ->
        (* Close both channels *)
        close_in in_channel;
        close_out out_channel;
        Printf.printf "File copied successfully from %s to %s\n" input_file output_file
    with e ->
      (* Handle any other exceptions *)
      close_in in_channel;
      close_out out_channel;
      raise e

