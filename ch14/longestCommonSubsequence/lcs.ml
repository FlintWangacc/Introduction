open Base

let lcs_length (x : char list) (y : char list) m n =
  let b = Array.make_matrix ~dimx:(m+1) ~dimy:(n+1) 0 in
  let c = Array.make_matrix ~dimx:(m+1) ~dimy:(n+1) 0 in
  let _ = List.iteri x
  ~f:(fun idx xi ->
        let i = idx + 1 in
        List.iteri y
          ~f:(
            fun idx yj ->
              let j = idx + 1 in
              if (Char.equal xi yj) then
                begin
                  c.(i).(j) <- (c.(i-1).(j-1) + 1);
                  b.(i).(j) <- 3
                end
              else 
                if c.(i-1).(j) >= c.(i).(j-1) then
                  begin
                    c.(i).(j) <- c.(i-1).(j);
                    b.(i).(j) <- 1
                  end
                else
                  begin
                    c.(i).(j) <- c.(i).(j-1);
                    b.(i).(j) <- 2
                  end
            )
  ) in
  c, b

let x = ['A'; 'B'; 'C'; 'B'; 'D'; 'A'; 'B']
let y = ['B'; 'D'; 'C'; 'A'; 'B'; 'A']

let print_matrix (matrix: int array array) =
  Array.iter ~f:(fun row ->
    Array.iter ~f:(fun element ->
      Caml.Printf.printf "%d\t" element
    ) row;
    Caml.Printf.printf "\n"
  ) matrix

let rec print_lcs b x i j =
  if Caml.(||) (i = 0) (j = 0) then
    ""
  else
    match b.(i).(j) with
      3 -> (print_lcs b x (i-1) (j-1)) ^ (x.(i-1) |> String.make 1)
    | 1 -> print_lcs b x (i-1) j
    | 2 -> print_lcs b x i (j-1)
    | _ -> failwith "illegal value"

let c, b = lcs_length x y (List.length x) (List.length y)

let () =
      (*Caml.Printf.printf "c:\n";
      print_matrix c;
      Caml.Printf.printf "b:\n";
      print_matrix b;
      Caml.Printf.printf "\n";*)
      print_lcs b (List.to_array x) (List.length x) (List.length y) |> Stdio.Out_channel.output_string Stdio.stdout;
      Caml.print_newline ()