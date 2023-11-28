type t

type locals

val empty : t

val get_name : string -> t -> string * t

val get_value : string -> t -> Ast.value

val add_local : string -> t -> string * t

val add_global : string -> t -> string * t

val set_value : string -> Ast.value -> t -> t

val get_locals : t -> locals

val with_locals : t -> locals -> t

val stdlib_load : t -> t
