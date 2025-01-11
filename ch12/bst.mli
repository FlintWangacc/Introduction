type 'a t

type 'a bst = { root : 'a t option ref}

val make_root : unit-> 'a bst ref

val make_node : 'a -> 'a t

val inorder_tree_walk : 'a t option -> ('a -> unit) -> unit

val tree_search : 'a bst -> 'a -> 'a t option

val iterative_tree_search : 'a t option -> 'a -> 'a t option

val tree_minimum : 'a t option -> 'a t option

val tree_maximum : 'a t option -> 'a t option

val tree_successor : 'a t -> 'a t option

val tree_insert : 'a bst ref -> 'a t -> unit

val tree_delete : 'a bst -> 'a t -> unit

val to_dot : 'a t option -> ('a -> int) -> string

val preorder_traverse : 'a bst -> ('a -> int) -> string

val export_to_dot : 'a bst -> ('a -> int) -> string -> unit

val get_key : 'a t -> 'a