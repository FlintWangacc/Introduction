type color = Red | Black

type ('a, 'b) rbtree = Leaf | Node of color * 'a * ' b * ('a, ' b) rbtree * ('a, 'b) rbtree


val is_empty : ('a, 'b) rbtree -> bool

val put : ('a, 'b) rbtree -> 'a -> 'b -> ('a, 'b) rbtree

val get : ('a, 'b) rbtree -> 'a -> 'b

(*val dump_dot : ('a, 'b) rbtree -> ('a -> string) -> ('b -> string) -> string*)

val dump_string : ('a, 'b) rbtree -> string -> ('a -> string) -> ('b -> string) -> unit

val delete_min : ('a, 'b) rbtree -> ('a, 'b) rbtree

val delete_max : ('a, 'b) rbtree -> ('a, 'b) rbtree

val delete : ('a, 'b) rbtree -> 'a -> ('a, 'b) rbtree
