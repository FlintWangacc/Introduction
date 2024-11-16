module MyStack =
    struct
        type 'a stack = {top: int; arr: 'a list }
        let makeStack num lst = { top = num; arr = lst}
        let stackEmpty = function {top = 0; _} -> true | _ -> false
        let push s q = makeStack (s.top + 1) (q :: s.arr)
        let pop s  = makeStack (s.top - 1) (List.tl s.arr)
    end

module MyQueue =
    struct
        type 'a queue = {size : int ref; head: int ref; tail : int ref; vector: 'a array}
        let makeQueue num = {size = ref num; head = ref 0; tail = ref 0; vector = Array.make num 0}
        let enqueue q x = q.vector.(!(q.tail)) <- x; if !(q.tail) = !(q.size) then q.tail := 1 else q.tail := !(q.tail) + 1
        let dequeue q = let x = q.vector.(!(q.head)) in if !(q.head) = !(q.size) then q.head := 1 else q.head := !(q.head) + 1; x
    end


module LinkList =
    struct
        type 'a element = { prev : 'a element option ref; key : 'a; next : 'a element option ref}
        type 'a linkList = {head : 'a element option ref}
        let makeLinklist = 
            {head  = ref None}
        let makeNode k = {prev = ref None; key = k; next = ref None}
        let listSearch llst k =
            let x = llst.head in
            let tl = match !x with
                    | None -> raise Not_found
                    | Some t -> ref t in
            while (!tl).key != k do
                match !((!tl).next) with
                | None -> raise Not_found
                | Some tn' -> tl := tn'
            done;
            (!tl).key

        let listPrepend llst x  =
            x.next := !(llst.head);
            x.prev := None;
            match !(llst.head) with
            | None -> llst.head := Some x
            | Some t -> t.prev := Some x; llst.head := Some x
            
        let listInsert x y =
            x.next := !(y.next);
            x.prev := Some y;
            match !(y.next) with
            | None ->y.next := Some x
            | Some t -> t.prev := Some x; y.next := Some x

        let listDelete llst x =
            match !(x.prev) with
            | None -> llst.head := !(x.next)
            | Some t -> t.next := !(x.next);
            match !(x.next) with
            | None -> ()
            | Some t -> t.prev := !(x.prev)
    end

let head = LinkList.makeLinklist

let t1 = LinkList.makeNode 1
let t2 = LinkList.makeNode 2

let _ = LinkList.listPrepend head t1

let _ = LinkList.listPrepend head t2

let getNext : 'a LinkList.element option -> 'a LinkList.element option = function
    | Some t -> !((t).next)
    | None -> raise Not_found

let getPrev : 'a LinkList.element option -> 'a LinkList.element option = function
    | Some t -> !((t).prev)
    | None -> raise Not_found

module LinkListSentinels =
struct
    type 'a element = { prev : 'a element option ref; key : 'a option; next : 'a element option ref}
    type 'a linkList = { head : 'a element }
    let makeLinklist () = 
        let t = {head  = {prev = ref None; key = None; next = ref None}} in
        t.head.prev := Some (t.head);
        t.head.next := Some (t.head);
        t
    
    let makeNode k =
        let t = {prev = ref None; key = Some k; next = ref None} in
        t.prev := (Some t);
        t.next := (Some t);
        t
    
    exception Illegal of string
    let nodePrev = function
        | {prev=prev; key=key;next=next} -> 
            (match !prev with
                | Some t -> t
                | None -> raise (Illegal "No previous")
            )
    let nodeNext = function
        | {prev=prev; key=key; next=next} ->
            (match !next with
                | Some t -> t
                | None -> raise (Illegal "No next")
            )

    let isHead t = match ((Option.get t).key) with
        | None -> true
        | Some t -> false

    let listSearch llst k =
        let ret = ref None in
        let optionEqual ao bo =
        match ao, bo with
        | None, None -> false
        | Some a, Some b -> a = b
        | _, _ -> false in
        let tl = ref (Option.get !(llst.head.next)) in
        let isHead t = match ((Option.get t).key) with
            | None -> true
            | Some t -> false
            in
        while  not (isHead (Some !tl)) do
        (*while optionEqual (!tl).key (Some k) do*)
            print_endline "loop 1";
            (match !tl with
            | {prev=prev;key=key;next=next} -> ((if (Option.get key) == k then (ret := Some !tl; (print_endline "match")) else ()); tl := (Option.get !((!tl).next)))
            | _ -> raise Not_found);
            print_endline "loop 1 end"
        done;
        ret

    let listDelete x =
        let prevx = match !(x.prev) with
                    | Some t -> t in
        let nextx = match !(x.next) with
                    | Some t -> t in
        prevx.next := !(x.next);
        nextx.prev := !(x.prev)
    
    let listInsert x y =
        let nexty = match !(y.next) with
                    | Some t -> t in
        let prevy = match !(y.prev) with
                    | Some t -> t in
        let nextx = match !(x.next) with
                    | Some t -> t in
        let prevx = match !(x.prev) with
                    | Some t -> t in
        x.next := !(y.next);
        x.prev := Some y;
        nexty.prev := Some x;
        y.next := Some x
end

let ts = LinkListSentinels.makeLinklist ()

let n1 = LinkListSentinels.makeNode 1

let n2 = LinkListSentinels.makeNode 2

let n3 = LinkListSentinels.makeNode 3

let _ = LinkListSentinels.listInsert n1 ts.head

let _ = LinkListSentinels.listInsert n2 ts.head

let _ = LinkListSentinels.listInsert n3 ts.head