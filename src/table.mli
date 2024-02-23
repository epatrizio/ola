type ('a, 'b) t

val empty : ('a, 'b) t

val add : ('a -> int option) -> 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t

val remove : ('a -> int option) -> 'a -> ('a, 'b) t -> ('a, 'b) t

val get : ('a -> int option) -> 'a -> ('a, 'b) t -> 'b option

val len : ('a, 'b) t -> int
