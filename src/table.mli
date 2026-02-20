module type KeyType = sig
  type t

  val int_key_opt : t -> int option

  val key_of_string : string -> t

  val string_of_val : t -> string option
end

module type S = sig
  exception Table_error of string

  val error : string -> 'a

  type kv

  type key =
    | Ikey of int
    | Kkey of kv

  type t

  val empty : t

  val is_empty : t -> bool

  val add : kv -> kv -> t -> t

  val remove : kv -> t -> t

  val key_exists : kv -> t -> bool

  val get : kv -> t -> kv option

  val border : (kv -> bool) -> t -> int

  val length : t -> int

  val next : kv option -> t -> (key * kv) option

  val inext : (kv -> bool) -> int -> t -> (int * kv) option

  val get_metatable : t -> t option

  val set_metatable : t -> t -> t

  val remove_metatable : t -> t

  val to_string : t -> string
end

module Make : functor (Key : KeyType) -> sig
  exception Table_error of string

  val error : string -> 'a

  type kv = Key.t

  type key =
    | Ikey of int
    | Kkey of kv

  type t

  val empty : t

  val is_empty : t -> bool

  val add : kv -> kv -> t -> t

  val remove : kv -> t -> t

  val key_exists : kv -> t -> bool

  val get : kv -> t -> kv option

  val border : (kv -> bool) -> t -> int

  val length : t -> int

  val next : kv option -> t -> (key * kv) option

  val inext : (kv -> bool) -> int -> t -> (int * kv) option

  val get_metatable : t -> t option

  val set_metatable : t -> t -> t

  val remove_metatable : t -> t

  val to_string : t -> string
end
