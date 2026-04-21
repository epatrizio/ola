(* MEMO: compare
   All functions ignore non-numeric keys in the tables given as arguments.
*)

let () = Random.self_init ()

(* TODO: rename KeyValueType *)
module type KeyType = sig
  type t

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

  (* TODO: remove *)
  (* type key =
    | Ikey of int
    | Kkey of kv *)

  type t

  val empty : unit -> t

  val is_empty : t -> bool

  val add : kv -> kv -> t -> t

  val remove : kv -> t -> t

  val key_exists : kv -> t -> bool

  val get : kv -> t -> kv option

  val border : t -> int

  val length : t -> int

  val next : kv option -> t -> (kv * kv) option

  val inext : int -> t -> (int * kv) option

  val get_metatable : t -> t option

  val set_metatable : t -> t -> t

  val remove_metatable : t -> t

  val to_string : t -> string
end

module Make (Key : KeyType) : S with type kv = Key.t = struct
  exception Table_error of string

  let error message = raise (Table_error message)

  type kv = Key.t

  (* type key =
    | Ikey of int
    | Kkey of kv *)

  type t =
    { table : (Key.t, Key.t) Hashtbl.t
    ; metatable : t option
    ; uid : int32
    }

  let empty () =
    let table = Hashtbl.create ~random:true 32 in
    { table; metatable = None; uid = Random.bits32 () }

  let add key value tbl =
    Hashtbl.replace tbl.table key value;
    tbl

  let remove key tbl =
    Hashtbl.remove tbl.table key;
    tbl

  let key_exists key tbl = Hashtbl.mem tbl.table key

  let get key tbl = Hashtbl.find_opt tbl.table key

  let length tbl = Hashtbl.length tbl.table

  (* "border" (~len) concept https://www.lua.org/manual/5.4/manual.html#3.4.7 *)
  let border tbl =
    let rec cpt idx tbl acc_len =
      let key_idx = Key.key_of_int idx in
      match get key_idx tbl with
      | None -> acc_len
      | Some v ->
        if Key.is_nil v then acc_len else cpt (idx + 1) tbl (acc_len + 1)
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
      let key_idx = Key.key_of_int (idx + 1) in
      match get key_idx tbl with None -> None | Some v -> Some (idx + 1, v)
    else None

  let get_metatable tbl = tbl.metatable

  let set_metatable meta_tbl tbl = { tbl with metatable = Some meta_tbl }

  let remove_metatable tbl = { tbl with metatable = None }

  let to_string tbl =
    let str_prefix =
      match get_metatable tbl with
      | Some mt ->
        begin match get (Key.key_of_string "__name") mt with
        | Some k ->
          begin match Key.string_of_val k with Some s -> s | None -> "table"
          end
        | None -> "table"
        end
      | None -> "table"
    in
    Format.sprintf "%s: %i" str_prefix (Int32.to_int tbl.uid)
end
