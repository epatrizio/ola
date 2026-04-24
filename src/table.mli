module type ValueType = sig
  type t

  val nil : t

  val is_nil : t -> bool

  val int_key_opt : t -> int option

  val key_of_int : int -> t

  val key_of_string : string -> t

  val string_of_val : t -> string option
end

module type S = sig
  exception Table_error of string

  val error : string -> 'a

  (* kv: key-value (same type for key and value) *)
  type kv

  type t

  val empty : unit -> t

  val is_empty : t -> bool

  val add : kv -> kv -> t -> t

  val add_meta_newindex : kv -> kv -> t -> (t, kv) result

  val remove : kv -> t -> t

  val key_exists : kv -> t -> bool

  val get : kv -> t -> (kv, kv) result

  val border : t -> int

  val length : t -> int

  val next : kv option -> t -> (kv * kv) option

  val inext : int -> t -> (int * kv) option

  val get_metatable : t -> t option

  val get_metatable_field : string -> t -> kv option

  val set_metatable : t -> t -> t

  val remove_metatable : t -> t

  val to_string : t -> string
end

module Make : functor (KeyValue : ValueType) -> sig
  exception Table_error of string

  val error : string -> 'a

  type kv = KeyValue.t

  type t

  val empty : unit -> t

  val is_empty : t -> bool

  val add : kv -> kv -> t -> t

  val add_meta_newindex : kv -> kv -> t -> (t, kv) result

  val remove : kv -> t -> t

  val key_exists : kv -> t -> bool

  val get : kv -> t -> (kv, kv) result

  val border : t -> int

  val length : t -> int

  val next : kv option -> t -> (kv * kv) option

  val inext : int -> t -> (int * kv) option

  val get_metatable : t -> t option

  val get_metatable_field : string -> t -> kv option

  val set_metatable : t -> t -> t

  val remove_metatable : t -> t

  val to_string : t -> string
end
