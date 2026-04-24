(* MEMO: compare
   All functions ignore non-numeric keys in the tables given as arguments.
*)

open Syntax

let () = Random.self_init ()

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

module Make (KeyValue : ValueType) : S with type kv = KeyValue.t = struct
  exception Table_error of string

  let error message = raise (Table_error message)

  type kv = KeyValue.t

  type t =
    { table : (kv, kv) Hashtbl.t
    ; metatable : t option
    ; uid : int32
    }

  let empty () =
    let table = Hashtbl.create ~random:true 32 in
    { table; metatable = None; uid = Random.bits32 () }

  let get_metatable tbl = tbl.metatable

  let set_metatable meta_tbl tbl = { tbl with metatable = Some meta_tbl }

  let remove_metatable tbl = { tbl with metatable = None }

  let add key value tbl =
    if KeyValue.is_nil value then Hashtbl.remove tbl.table key
    else Hashtbl.replace tbl.table key value;
    tbl

  let remove key tbl =
    Hashtbl.remove tbl.table key;
    tbl

  let key_exists key tbl = Hashtbl.mem tbl.table key

  let get_metatable_field name tbl =
    let+ mt = get_metatable tbl in
    let key = KeyValue.key_of_string name in
    Hashtbl.find_opt mt.table key

  let get key tbl =
    if key_exists key tbl then
      let val_opt = Hashtbl.find_opt tbl.table key in
      Option.to_result ~none:KeyValue.nil val_opt
    else
      match get_metatable_field "__index" tbl with
      | None -> Error KeyValue.nil
      | Some mt -> Error mt

  let length tbl = Hashtbl.length tbl.table

  (* "border" (~len) concept https://www.lua.org/manual/5.4/manual.html#3.4.7 *)
  let border tbl =
    let rec cpt idx tbl acc_len =
      let key_idx = KeyValue.key_of_int idx in
      match get key_idx tbl with
      | Error _ -> acc_len
      | Ok v ->
        if KeyValue.is_nil v then acc_len else cpt (idx + 1) tbl (acc_len + 1)
    in
    cpt 1 tbl 0

  let is_empty tbl = length tbl = 0

  (* https://www.lua.org/manual/5.4/manual.html#6.1
   TODO: Warning spec not fully implemented *)
  let next key_opt tbl =
    let first_seq seq =
      match seq () with Seq.Nil -> None | Seq.Cons ((k, v), _) -> Some (k, v)
    in
    let rec next_seq key seq =
      match seq () with
      | Seq.Nil -> None
      | Seq.Cons ((k, _), tl_seq) ->
        if key = k then first_seq tl_seq else next_seq key tl_seq
    in
    let seq_tbl = Hashtbl.to_seq tbl.table in
    match seq_tbl () with
    | Seq.Nil -> None
    | Seq.Cons ((k, v), _tl_seq) -> (
      match key_opt with
      | None -> Some (k, v)
      | Some key ->
        if key_exists key tbl then next_seq key seq_tbl
        else error "invalid key to 'next'" )

  let inext idx tbl =
    let border = border tbl in
    if idx < border then
      let key_idx = KeyValue.key_of_int (idx + 1) in
      match get key_idx tbl with Error _ -> None | Ok v -> Some (idx + 1, v)
    else None

  let add_meta_newindex key value tbl =
    if key_exists key tbl then Ok (add key value tbl)
    else
      match get_metatable_field "__newindex" tbl with
      | None -> Ok (add key value tbl)
      | Some mt -> Error mt

  let to_string tbl =
    let str_prefix =
      match get_metatable_field "__name" tbl with
      | None -> "table"
      | Some k -> (
        match KeyValue.string_of_val k with Some s -> s | None -> "table" )
    in
    Format.sprintf "%s: %i" str_prefix (Int32.to_int tbl.uid)
end
