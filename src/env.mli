exception Env_error of unit option * string

type 'a t

type locals

val empty : unit -> 'a t

val get_name : string -> 'a -> 'a t -> string * 'a t

val get_value : string -> 'a t -> ('a, 'b option * string) result

val add_local : string -> 'a -> 'a t -> string * 'a t

val add_global : string -> 'a -> 'a t -> string * 'a t

val add_global_force : string -> 'a -> 'a t -> 'a t

val add_local_force : string -> 'a -> 'a t -> 'a t

val update_value : string -> 'a -> 'a t -> (unit, 'c option * 'b) result

val add_value : string -> 'a -> 'a t -> ('a t, 'c option * 'b) result

val get_locals : 'a t -> locals

val with_locals : 'a t -> locals -> 'a t
