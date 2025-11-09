class graph (filename : string) =
    object (self)
        val mutable adjacency_list : int list array = [||]
        val mutable num_vertices = 0
        val mutable is_directed = false

        initializer
            self#read_file filename
        
        method private read_file (filename : string) =
            let channel = open_in filename in
            try
                let first_line = input_line channel in
                let parts = String.split_on_char ' ' first_line in
                num_vertices <- int_of_string (List.nth parts 0);
                is_directed <- (int_of_string (List.nth parts 1) = 1);
                adjacency_list <- Array.make num_vertices [];
                let rec read_edges () =
                    try 
                        let line = input_line channel in
                        if line <> "" then (
                            let edge_parts = String.split_on_char ' ' line in
                            let u = int_of_string (List.nth edge_parts 0) in
                            let v = int_of_string (List.nth edge_parts 1) in
                            adjacency_list.(u - 1) <- v :: adjacency_list.(u - 1);
                            if not is_directed then
                                adjacency_list.(v - 1) <- u :: adjacency_list.(v - 1)
                        );
                        read_edges ()
                    with End_of_file -> ()
                in
                read_edges ();
                close_in channel
            with e ->
                close_in channel;
                raise e

        method get_adjacency_list = adjacency_list
        method get_num_vertices = num_vertices
        method is_directed = is_directed

        method print_graph =
            Printf.printf "Graph (%d vertices, %s):\n" num_vertices
                (if is_directed then "directed" else "undirected");
            for i = 0 to num_vertices - 1 do
                Printf.printf "Vertex %d: " (i + 1);
                List.iter (fun x -> Printf.printf "%d " x) adjacency_list.(i);
                print_endline ""
            done
    end

let () =
    let g = new graph "graph.txt" in
    g#print_graph