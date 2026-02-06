(* MEMO: compare
   All functions ignore non-numeric keys in the tables given as arguments.
*)

module type KeyType = sig
  type t

  val int_key_opt : t -> int option
end

module type S = sig
  exception Table_error of string

  val error : string -> 'a

  (* kv: key-value (same type for key and value) *)
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
end

module Make (Key : KeyType) : S with type kv = Key.t = struct
  exception Table_error of string

  let error message = raise (Table_error message)

  type kv = Key.t

  type key =
    | Ikey of int
    | Kkey of kv

  type t =
    { ilist : (int * kv) list (* ordered list on key *)
    ; klist : (kv * kv) list
    ; metatable : t option
    }

  let empty = { ilist = []; klist = []; metatable = None }

  let rec k_add key value ktbl =
    match ktbl with
    | [] -> [ (key, value) ]
    | (k, _v) :: tl when k = key -> (k, value) :: tl
    | (k, v) :: tl -> (k, v) :: k_add key value tl

  let rec i_add idx value itbl =
    match itbl with
    | [] -> [ (idx, value) ]
    | (i, _v) :: tl when i = idx -> (idx, value) :: tl
    | (i, v) :: tl when i > idx -> (idx, value) :: (i, v) :: tl
    | (i, v) :: tl -> (i, v) :: i_add idx value tl

  let add key value tbl =
    match Key.int_key_opt key with
    | Some idx -> { tbl with ilist = i_add idx value tbl.ilist }
    | None -> { tbl with klist = k_add key value tbl.klist }

  let i_remove idx tbl = { tbl with ilist = List.remove_assoc idx tbl.ilist }

  let k_remove key tbl = { tbl with klist = List.remove_assoc key tbl.klist }

  let remove key tbl =
    match Key.int_key_opt key with
    | Some idx -> i_remove idx tbl
    | None -> k_remove key tbl

  let i_get idx tbl = List.assoc_opt idx tbl.ilist

  let k_get key tbl = List.assoc_opt key tbl.klist

  let key_exists key tbl =
    match Key.int_key_opt key with
    | Some idx -> Option.is_some (i_get idx tbl)
    | None -> Option.is_some (k_get key tbl)

  let get key tbl =
    match Key.int_key_opt key with
    | Some idx -> i_get idx tbl
    | None -> k_get key tbl

  (* "border" (~len) concept https://www.lua.org/manual/5.4/manual.html#3.4.7 *)
  let border fun_border_up tbl =
    let rec cpt idx itbl acc_len =
      match List.assoc_opt idx itbl with
      | None -> acc_len
      | Some v ->
        if fun_border_up v then acc_len else cpt (idx + 1) itbl (acc_len + 1)
    in
    cpt 1 tbl.ilist 0

  let length tbl = List.length tbl.ilist + List.length tbl.klist

  let is_empty tbl = length tbl = 0

  let first__elt tbl = match tbl with [] -> None | e :: _ -> Some e

  let first_i_elt tbl =
    match first__elt tbl.ilist with
    | Some (idx, v) -> Some (Ikey idx, v)
    | None -> None

  let first_k_elt tbl =
    match first__elt tbl.klist with
    | Some (k, v) -> Some (Kkey k, v)
    | None -> None

  (* Pre-condition: table isn't empty
   Rule: int key first *)
  let first_elt tbl =
    match first_i_elt tbl with Some _ as elt -> elt | None -> first_k_elt tbl

  (* Pre-condition 1: table isn't empty
   Pre-condition 2: key exists *)
  let rec next_elt key tbl =
    match Key.int_key_opt key with
    | Some idx -> begin
      match tbl.ilist with
      | [] -> assert false (* PC2 *)
      | (i, _) :: tl when idx = i -> first_elt { tbl with ilist = tl }
      | _ :: tl -> next_elt key { tbl with ilist = tl }
    end
    | None -> (
      match tbl.klist with
      | [] -> assert false (* PC2 *)
      | (k, _) :: tl when key = k -> first_k_elt { tbl with klist = tl }
      | _ :: tl -> next_elt key { tbl with klist = tl } )

  (* https://www.lua.org/manual/5.4/manual.html#6.1
   TODO: Warning spec not fully implemented *)

  let next key tbl =
    (* first level checks : empty table and key existence *)
    if is_empty tbl then None
    else
      match key with
      | Some key -> begin
        match Key.int_key_opt key with
        | Some idx -> begin
          match i_get idx tbl with
          | Some _ -> next_elt key tbl
          | None -> error ("invalid key to 'next': " ^ Int.to_string idx)
        end
        | None -> begin
          match k_get key tbl with
          | Some _ -> next_elt key tbl
          | None -> error "invalid key to 'next'"
        end
      end
      | None -> first_elt tbl

  let inext fun_border_up idx tbl =
    let border = border fun_border_up tbl in
    if idx < border then
      match i_get (idx + 1) tbl with
      | Some v -> Some (idx + 1, v)
      | None -> None
    else None

  let get_metatable tbl = tbl.metatable

  let set_metatable meta_tbl tbl = { tbl with metatable = Some meta_tbl }

  let remove_metatable tbl = { tbl with metatable = None }
end
