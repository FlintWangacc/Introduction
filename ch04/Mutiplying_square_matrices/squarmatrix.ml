type 'a squareMatrix = 'a list list

let isSquareMatrix m =
  let n = List.length m in
  List.for_all (fun t -> n = List.length t) m

let rownth m n =
  List.map (fun lst -> List.nth lst n) m
  
let matrixMultiply am bm =
  let n = List.length am in
  let result = ref [] in
  for i = 1 to n do
    let row = ref [] in
    for j = 1 to n do
      let c = ref 0 in
      let sa = List.nth am (i-1) in
      let tb = rownth bm (j-1) in
      for k = 1 to n do
        let t1 = List.nth sa (k-1) in
        let t2 = List.nth tb (k-1) in
        (*Printf.printf "t1:%d, t2:%d, k:%d" t1 t2 k;
        print_newline ();*)
        c := !c + t1 * t2;
        (*Printf.printf "c:%d" !c;
        print_newline ();*)
      done;
      (*print_int !c;
      print_endline;*)
      row := List.append !row [!c]
    done;
    result := List.append !result [!row]
  done;
  !result

let splitMatrix mx =
  let upperHalf = List.filteri (fun idx _ -> idx < (List.length mx) / 2) mx in
  let downHalf = List.filteri (fun idx _ -> idx >= (List.length mx) / 2) mx in
  let getLeft tmp = List.map (fun t -> List.filteri (fun idx _ -> idx < (List.length t) / 2) t) tmp in
  let getRight tmp = List.map (fun t -> List.filteri (fun idx _ -> idx >= (List.length t) / 2) t) tmp in
  getLeft upperHalf, getRight upperHalf, getLeft downHalf, getRight downHalf

let matrixAdd m1 m2 =
  let addRow r1 r2 = List.map2 (fun x y -> x + y) r1 r2 in
  List.map2 addRow m1 m2

let matrixSub m1 m2 =
  let subRow r1 r2 = List.map2 (fun x y -> x - y) r1 r2 in
  List.map2 subRow m1 m2

let mergeMatrix c11 c12 c21 c22 =
  let upperHalf = List.map2 (fun a b -> a @ b) c11 c12 in
  let downHalf = List.map2 (fun a b -> a @ b) c21 c22 in
  upperHalf @ downHalf

  (*let debugList t = print_string "["; List.iter (fun tmp -> Stdio.printf "%d " tmp) t; print_string "]";;*)

let rec matrixMultiplyRecursive matrixA matrixB =
  if List.nth matrixA 0 |> List.length = 1 then
    (*let () = print_endline "=====A======";
    List.iter debugList matrixA;
    print_endline "============";
    print_endline "=====B======";
    List.iter debugList matrixB;
    print_endline "============" in*)
    let getSingleElem x = List.nth (List.nth x 0) 0 in
    let a11 = getSingleElem matrixA in
    let b11 = getSingleElem matrixB in
    [[a11 * b11]]
  else
    let a11,a12,a21,a22 = splitMatrix matrixA in
    let b11,b12,b21,b22 = splitMatrix matrixB in
    let c11 = matrixAdd (matrixMultiplyRecursive a11 b11) (matrixMultiplyRecursive a12 b21) in
    let c12 = matrixAdd (matrixMultiplyRecursive a11 b12) (matrixMultiplyRecursive a12 b22) in
    let c21 = matrixAdd (matrixMultiplyRecursive a21 b11) (matrixMultiplyRecursive a22 b21) in
    let c22 = matrixAdd (matrixMultiplyRecursive a21 b12) (matrixMultiplyRecursive a22 b22) in
    mergeMatrix c11 c12 c21 c22

let rec strassenMatrixMultiplyRecursive matrixA matrixB =
  if List.nth matrixA 0 |> List.length = 1 then
    let getSingleElem x = List.nth (List.nth x 0) 0 in
    let a11 = getSingleElem matrixA in
    let b11 = getSingleElem matrixB in
    [[a11 * b11]]
  else
    let a11,a12,a21,a22 = splitMatrix matrixA in
    let b11,b12,b21,b22 = splitMatrix matrixB in
    let s1 = matrixSub b12 b22 in
    let s2 = matrixAdd a11 a12 in
    let s3 = matrixAdd a21 a22 in
    let s4 = matrixSub b21 b11 in
    let s5 = matrixAdd a11 a22 in
    let s6 = matrixAdd b11 b22 in
    let s7 = matrixSub a12 a22 in
    let s8 = matrixAdd b21 b22 in
    let s9 = matrixSub a11 a21 in
    let s10 = matrixAdd b11 b12 in
    let p1 = strassenMatrixMultiplyRecursive a11 s1 in
    let p2 = strassenMatrixMultiplyRecursive s2 b22 in
    let p3 = strassenMatrixMultiplyRecursive s3 b11 in
    let p4 = strassenMatrixMultiplyRecursive a22 s4 in
    let p5 = strassenMatrixMultiplyRecursive s5 s6 in
    let p6 = strassenMatrixMultiplyRecursive s7 s8 in
    let p7 = strassenMatrixMultiplyRecursive s9 s10 in
    let c11 = matrixAdd (matrixSub (matrixAdd p5 p4) p2) p6 in
    let c12 = matrixAdd p1 p2 in
    let c21 = matrixAdd p3 p4 in
    let c22 = matrixSub (matrixSub (matrixAdd p5 p1) p3) p7 in
    mergeMatrix c11 c12 c21 c22

let printSquareMatrix t =
  let printIntList lst =
    Printf.printf "[";
    List.iteri (fun idx t -> if (idx + 1) < List.length lst then
                                Printf.printf "%d, " t
                              else
                                Printf.printf "%d" t) lst;
    Printf.printf "]\n" in
  print_endline "=======Matrix========";
  List.iter printIntList t;
  print_endline "====================="
  
let () =
  let t = [[1;2;3;4]; [5;6;7;8]; [9; 10; 11; 12]; [13; 14; 15; 16]] in
  let r = strassenMatrixMultiplyRecursive t t in
  printSquareMatrix r