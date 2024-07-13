type 'a t

type locals

val empty : unit -> 'a t

val get_name : string -> 'a ref -> 'a t -> string * 'a t

val get_value : string -> 'a t -> 'a

val add_local : string -> 'a ref -> 'a t -> string * 'a t

val add_global : string -> 'a ref -> 'a t -> string * 'a t

val add_global_force : string -> 'a ref -> 'a t -> 'a t

val update_value : string -> 'a ref -> 'a t -> unit

val add_value : string -> 'a ref -> 'a t -> 'a t

val get_locals : 'a t -> locals

val with_locals : 'a t -> locals -> 'a t
