type 'a t

val create : int -> ('a -> int -> int) -> 'a t

val hashInsert : 'a t -> 'a -> int

val hashSearch : 'a t -> 'a -> int option