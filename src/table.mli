type ('a, 'b) t

val empty : ('a, 'b) t

val add : 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t

val get : 'a -> ('a, 'b) t -> 'b option

val len : ('a, 'b) t -> int

(* val remove : 'a -> t -> t *)
