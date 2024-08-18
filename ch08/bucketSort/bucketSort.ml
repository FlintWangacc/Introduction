type 'a listElem = {elem : 'a; mutable next : 'a listElem option}

let rec insertLst e = function
  | None -> (Some {elem = e; next = None})
  | Some {elem= t; next=remain} as tmp->
      if e < t then
        (Some {elem=e; next=tmp})
      else
        (Some {elem = t; next= (insertLst e remain)})

let rec getTail e = match e.next with
   | None ->  e
   | Some t-> getTail t

let concatLst l1 l2 =
  let l1head = l1 in
  let l1t = getTail l1 in
  match (!l2) with
  | None -> l1head
  | e -> l1t.next <- (e); l1head

let rec foldLst lst = function
  | None -> lst
  | Some t -> foldLst (t.elem::lst) t.next

let bucketSort arr n =
  let t = {elem =0.; next= None} in
  let b = List.init n (fun _ -> ref (None)) in
  List.iter
    (fun t ->
        let idx = 10. *. t |> int_of_float in
        let head = List.nth b idx in
        match !head with
        | None -> head := insertLst t !head
        | Some _ as ins -> head := insertLst t ins) arr;
  foldLst [] (List.fold_left concatLst t b).next |> List.rev

let _ = let t = bucketSort [0.78; 0.17; 0.39; 0.26; 0.72; 0.94; 0.21; 0.12; 0.23; 0.68] 10 in
        List.iter (fun t -> Printf.printf "%f\t" t) t; Printf.printf "\n"