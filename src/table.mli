exception Table_error of string

type 'a key =
  | Ikey of int
  | Kkey of 'a

type ('a, 'b) t

val empty : ('a, 'b) t

val is_empty : ('a, 'b) t -> bool

val add : ('a -> int option) -> 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t

val remove : ('a -> int option) -> 'a -> ('a, 'b) t -> ('a, 'b) t

val key_exists : ('a -> int option) -> 'a -> ('a, 'b) t -> bool

val get : ('a -> int option) -> 'a -> ('a, 'b) t -> 'b option

val len : ('a, 'b) t -> int

val length : ('a, 'b) t -> int

val next : 'a key option -> ('a, 'b) t -> ('a key * 'b) option

val inext : int -> ('a, 'b) t -> (int * 'b) option

val get_metatable : ('a, 'b) t -> ('a, 'b) t option

val set_metatable : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t

val remove_metatable : ('a, 'b) t -> ('a, 'b) t
