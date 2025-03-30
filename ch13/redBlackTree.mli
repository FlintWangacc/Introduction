
val is_empty : unit -> bool

val put : 'a -> 'b -> unit

val get : 'a -> 'b

(*val dump_dot : ('a, 'b) rbtree -> ('a -> string) -> ('b -> string) -> string

val dump_string : ('a, 'b) rbtree -> string -> ('a -> string) -> ('b -> string) -> unit*)

val delete_min : unit -> unit

val delete_max : unit -> unit

val delete : 'a -> unit
