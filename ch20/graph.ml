open Stdlib

type color = WHITE | GRAY | BLACK
type vertex_info = { color : color ref; d : int ref ; f : int ref; p : vertex_info option ref; idx : int}

class graph (filename : string option) =
    object (self)

        val mutable adjacency_list : int list array = [||]
        val mutable num_vertices = 0
        val mutable is_directed = false
        val mutable vertexes : vertex_info array = [||]
        val mutable top_order : int list = []

        method get_num_vertices = num_vertices

        method set_num_vertices n =
            num_vertices <- n;
            adjacency_list <- Array.make (n + 1) [];  (* 通常需要重新初始化 *)
            self#init_vertexes ()

        method set_directed d =
            is_directed <- d

        method private vertexes_to_int vv =
            match (Array.find_index (fun t -> t = vv) vertexes) with
            | Some  idx -> idx
            | None -> failwith "Not found"

        method private init_vertexes () =
            vertexes <- Array.init
                        (num_vertices + 1)
                        (fun t -> {color = ref WHITE; d = ref max_int; f = ref max_int; p = ref None; idx = t})

        initializer
            match filename with
            | Some f -> self#read_file2 f;
                        self#init_vertexes ()
            | None -> ()


        method toplogical_order () : int list =
            top_order

        method add_edge u v directed =
            adjacency_list.(u) <- v :: adjacency_list.(u);
            if not directed then
                adjacency_list.(v) <- u :: adjacency_list.(v)

        method  reverse () =
            let rg = new graph None in
            let _ = rg#set_num_vertices num_vertices in
            let _ = rg#set_directed is_directed in
            let add_edge idx lst =
                List.iter (fun t -> rg#add_edge t idx self#is_directed) lst in
           Array.iteri (fun idx t -> if idx > 0 then add_edge idx t else ()) self#get_adjacency_list ;
            rg


        method private read_file2 (filename : string) =
            let channel = open_in filename in
            match In_channel.input_lines channel with
            | hd:: tl ->
                (
                    let handle_first line =
                        match String.split_on_char ' ' line |> List.map (fun t -> int_of_string t) with
                        | [n ; d] -> n, d <> 0
                        | _ -> failwith "illegal first line" in
                    let n, directed = handle_first hd in
                    let handle_line line =
                        let _ = print_string line;print_newline () in
                        match String.split_on_char ' ' line |> List.map int_of_string with
                        | [u; v] -> self#add_edge u v directed
                        | _ -> failwith "Illegal line content" in
                    let _ = adjacency_list <- Stdlib.Array.make (n + 1) []; num_vertices <- n; is_directed <- directed in
                    List.iter (fun t -> handle_line t) tl
                )
            | _ -> failwith "illegal file content"


        (*method private read_file (filename : string) =
            let channel = open_in filename in
            try
                let first_line = input_line channel in
                let parts = String.split_on_char ' ' first_line in
                num_vertices <- int_of_string (List.nth parts 0);
                is_directed <- (int_of_string (List.nth parts 1) = 1);
                adjacency_list <- Array.make (num_vertices + 1) [];
                let rec read_edges () =
                    try
                        let line = input_line channel in
                        if line <> "" then (
                            let edge_parts = String.split_on_char ' ' line in
                            let u = int_of_string (List.nth edge_parts 0) in
                            let v = int_of_string (List.nth edge_parts 1) in
                            adjacency_list.(u) <- v :: adjacency_list.(u);
                            if not is_directed then
                                adjacency_list.(v) <- u :: adjacency_list.(v)
                        );
                        read_edges ()
                    with End_of_file -> ()
                in
                read_edges ();
                close_in channel
            with e ->
                close_in channel;
                raise e*)


        method get_adjacency_list = adjacency_list
        method is_directed = is_directed

        method print_graph =
            Stdlib.Printf.printf "Graph (%d vertices, %s):\n" num_vertices
                (if is_directed then "directed" else "undirected");
            for i = 0 to num_vertices  do
                Stdlib.Printf.printf "Vertex %d: " (i);
                List.iter (fun x -> Stdlib.Printf.printf "%d " x) adjacency_list.(i);
                print_endline ""
            done


        method bfs2 (s : int) =
            vertexes.(s) <- {color = ref GRAY; d = ref 0; f = ref 0; p = ref None; idx = s};
            let rec bfs q =
                let q' = Stdlib.Queue.create () in
                match Stdlib.Queue.take_opt q with
                | None -> ()
                | Some  e ->
                    (*let _ = Printf.printf "%d\n" e.idx in*)
                    (
                    let handle_v u = function
                    | {color = color; d = d; p = p} as v ->
                        match !color with
                        | WHITE -> (
                            v.color := GRAY;
                            v.d := !(u.d) + 1;
                            v.p := Some u;
                            Stdlib.Queue.add v q'
                        )
                        | _ -> () in
                    let adj_lst = adjacency_list.(e.idx) |> List.map (fun i -> vertexes.(i)) in
                    List.iter (fun t -> handle_v e t) adj_lst; e.color := BLACK; bfs q'
                ) in
                let q = Stdlib.Queue.create () in  let _ = Stdlib.Queue.add vertexes.(s) q in
                bfs q

        method bfs (s : int)=
            vertexes.(s) <- {color = ref GRAY; d = ref 0; f = ref 0; p = ref None; idx = s};
            let bfs_queue = Stdlib.Queue.create () in
            let handle_v u = function
            | {color = color; d = d; p = p} as v ->
                match !color with
                | WHITE -> (
                    v.color := GRAY;
                    v.d := !(u.d) + 1;
                    v.p := Some u;
                    Stdlib.Queue.add v bfs_queue
                )
            | _ -> () in
            let _ = Stdlib.Queue.add vertexes.(s) bfs_queue in
            while not (Stdlib.Queue.is_empty bfs_queue) do
                let u = Stdlib.Queue.take bfs_queue in
                let adj_lst = adjacency_list.(u.idx) |> List.map (fun i -> vertexes.(i)) in
                List.iter (fun t -> handle_v u t) adj_lst;u.color := BLACK
            done;
            ()

        method dfs () =
            let time = ref 0 in
            let rec dfs_visit u time =
                time := !time + 1;
                u.d := !time;
                u.color := GRAY;
                let handle_v v u  =
                    if !(v.color) == WHITE then
                        (v.p := Some u;
                        dfs_visit  v time)
                    else () in

                match self#vertexes_to_int u with
                | u_idx ->
                    let adj_lst = adjacency_list.(u_idx) |> List.map (fun i -> vertexes.(i)) in
                    List.iter (fun v -> handle_v v u ) adj_lst; Printf.printf "u_idx:%d\n" u_idx;
                time := !time + 1;
                u.f := !time;
                let _ = Printf.printf "insert %d front\n" (self#vertexes_to_int u) in
                top_order <- self#vertexes_to_int u :: top_order;
                u.color := BLACK
            in
                Stdlib.Array.iteri (fun idx v ->
                    if idx > 0 then
                        if !(v.color) = WHITE then dfs_visit v time
                    else ()
                ) vertexes

        method print_path (s : int) (v : int) =
            if v = s then
                [s]
            else
                let pv = vertexes.(v).p in
                match !(pv) with
                | None -> failwith "no path"
                | Some p -> let vpv = self#vertexes_to_int p in (self#print_path s vpv) @  [v]


        (*method private generate_dot_content (add_line : string -> unit) =
            let graph_type = if is_directed then "digraph" else "graph" in
            let edge_op = if is_directed then "->" else "--" in

            add_line (Printf.sprintf "%s G {" graph_type);
            add_line "    layout=dot;";
            add_line "    rankdir=LR;";
            add_line "    node [shape=circle];";

            (* Add all vertices *)
            for i = 1 to num_vertices do
                add_line (Printf.sprintf "    %d;" i)
            done;

            (* Add edges - since adjacency lists are sorted, edges will be in order *)
            for i = 1 to num_vertices do
                let u = i in
                let neighbors =
                    if is_directed then adjacency_list.(i)
                    else List.filter (fun v -> v >= u) adjacency_list.(i)
                in
                List.iter (fun v ->
                    add_line (Printf.sprintf "    %d %s %d;" u edge_op v)
                ) neighbors
            done;

            add_line "}"*)
        method private generate_dot_content (add_line : string -> unit) =
            let graph_type = if is_directed then "digraph" else "graph" in
            add_line (Printf.sprintf "%s G {" graph_type);
            add_line "    layout=dot;";
            add_line "    rankdir=LR;";
            add_line "    node [shape=circle];";

            (* Add all vertices *)
            for i = 1 to num_vertices do
                add_line (Printf.sprintf "    %d;" i)
            done;

            (* Add edges - CRITICAL FIX: Different handling for directed vs undirected *)
            if is_directed then
                (* For directed graphs: all edges get arrows *)
                for i = 0 to num_vertices do
                    let u = i in
                    List.iter (fun v ->
                        add_line (Printf.sprintf "    %d -> %d;" u v)
                    ) adjacency_list.(i)
                done
            else
                (* For undirected graphs: only add edges where v >= u to avoid duplicates *)
                for i = 0 to num_vertices do
                    let u = i in
                    let neighbors = List.filter (fun v -> v >= u) adjacency_list.(i) in
                    List.iter (fun v ->
                        add_line (Printf.sprintf "    %d -- %d;" u v)
                    ) neighbors
            done;

            add_line "}"

        (* Generate DOT format representation of the graph *)
        method to_dot ?(output_file="graph.dot") () =
            let channel = open_out output_file in
            self#generate_dot_content (fun line ->
                Printf.fprintf channel "%s\n" line
            );
            close_out channel;
            output_file

        (* Alternative method that returns the DOT string without writing to file *)
        method to_dot_string () =
            let buffer = Buffer.create 1024 in
            self#generate_dot_content (fun line ->
                Buffer.add_string buffer line;
                Buffer.add_char buffer '\n'
            );
            Buffer.contents buffer

        (* Generate picture from DOT file using Graphviz *)
        method dump_picture ?(output_file="graph.png") ?(format="png") () =
            let dot_file = self#to_dot () in
            let command = Printf.sprintf "dot -T%s %s -o %s" format dot_file output_file in
            let result = Sys.command command in
            if result = 0 then
                Printf.printf "Graph picture saved as: %s\n" output_file
            else
                Printf.eprintf "Error: Failed to generate picture. Make sure Graphviz is installed.\n";
            output_file

    end

class digraph (filename : string option) =
    object (self)
        inherit graph None as super

        method private read_digraph_format (file : string) =
            let channel = open_in file in
            match In_channel.input_lines channel with
            | hd1::hd2:: tl ->
                (
                    let nv, ne = int_of_string hd1, int_of_string hd2 in
                    let handle_line line =
                        let _ = print_string line;print_newline () in
                        match String.split_on_char ' ' line |> List.map int_of_string with
                        | [u; v] -> self#add_edge u v true
                        | _ -> failwith "Illegal line content" in
                    let _ = adjacency_list <- Stdlib.Array.make (nv + 1) []; num_vertices <- nv; is_directed <- true in
                    List.iter (fun t -> handle_line t) tl
                )
            | _ -> failwith "illegal file content"

        initializer
            match filename with
            | Some f ->
                self#read_digraph_format f;
                self#init_vertexes ()
            | None ->
                self#set_directed true
    end

class depthFirstOrder (g: graph) =
    let num_vertices = g#get_num_vertices + 1 in
    object (self)
        val mutable pre : int array = Array.make num_vertices 0
        val mutable post : int array = Array.make num_vertices 0
        val mutable postorder : int list = []
        val mutable preorder : int list = []
        val mutable marked : bool array = Array.make num_vertices false
        val mutable preCounter : int = 0
        val mutable postCounter : int = 0
        val mutable reversePost : int list = []

        initializer
            for v = 0 to num_vertices - 1 do
                if not marked.(v) then self#dfs g v
            done

        method dfs (g : graph) (v : int) =
            marked.(v) <- true;
            let _ = preCounter <- preCounter + 1 in
            pre.(v) <- preCounter;
            preorder <- v :: preorder;
            let adj = g#get_adjacency_list.(v) in
            List.iter
            (fun w -> if not marked.(w) then self#dfs g w)
            adj;
            reversePost <- v::reversePost;
            let _ = postCounter <- postCounter + 1 in
            post.(v) <- postCounter

        method post () =
            List.rev reversePost

        method reversePost () =
            reversePost

    end

class kosarajuSCC (dg : digraph) =
    let num_vertices = dg#get_num_vertices + 1 in
    object (self)
        val mutable marked : bool array = Array.make num_vertices false
        val mutable id : int array = Array.make num_vertices 0
        val mutable count : int = 0

        method dfs (dg : digraph) (v : int) =
            marked.(v) <- true;
            id.(v) <- count;
            let adj = dg#get_adjacency_list.(v) in
            List.iter
            (fun w -> if not marked.(w) then self#dfs dg w)
            adj

        method stronglyConnected v w =
            id.(v) = id.(w)

        method id v =
            id.(v)

        method count () =
            count

        initializer
            let order = new depthFirstOrder (dg#reverse ()) in
            let rp = order#reversePost () in
            List.iter
            (fun s -> if not marked.(s) then (count <- count + 1; self#dfs dg s))
            rp
    end

(*let () =
    let filename = Some "/mnt/c/Users/hmsjw/Introduction/ch20/deepseek.txt" in
    let g = new graph filename in
    g#print_graph;
    let _ = g#dfs () in
    List.iter (fun t -> Printf.printf "%d " t) (g#print_path 1 7); print_newline ();
    let dot_file = g#to_dot () in
    Printf.printf "DOT file generated: %s\n" dot_file;

    (* Generate picture (requires Graphviz) *)
    let picture_file = g#dump_picture () in
    Printf.printf "Picture generated: %s\n" picture_file;
    let top = g#toplogical_order ()  in
    List.iter (fun t -> Printf.printf "%d " t) top;
    print_newline ();
    let rg = g#reverse () in
    let picture_file = rg#dump_picture ~output_file:"rg.png" ~format:"png" () in
    rg#print_graph;
    Printf.printf "Picture generated: %s\n" picture_file;
    let order = new depthFirstOrder rg in
    let rp = order#reversePost () in
    List.iter (fun t -> Printf.printf "%d " t)  rp*)

let () =
    let filename = Some "tinyDG.txt" in
    let dg = new digraph filename in
    dg#print_graph;
    let _= dg#dump_picture () in ();
    let scc = new kosarajuSCC dg in
    let m = scc#count () in
    let components = Array.init (m + 1) (fun _ -> []) in
    for v = 0 to dg#get_num_vertices  do
        components.(scc#id(v)) <- v :: components.(scc#id(v))
    done;
    Array.iter
    (fun lst -> List.iter (fun t -> Printf.printf "%d " t) lst; print_newline ())
    components
