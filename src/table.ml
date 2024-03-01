(* MEMO: compare
   All functions ignore non-numeric keys in the tables given as arguments.
*)

exception Table_error of string

let error message = raise (Table_error message)

type 'a key =
  | Ikey of int
  | Kkey of 'a

type ('a, 'b) t =
  { ilist : (int * 'b) list (* ordered list on key *)
  ; klist : ('a * 'b) list
  }

let empty = { ilist = []; klist = [] }

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

let add get_int_key_opt key value tbl =
  match get_int_key_opt key with
  | Some idx -> { ilist = i_add idx value tbl.ilist; klist = tbl.klist }
  | None -> { ilist = tbl.ilist; klist = k_add key value tbl.klist }

let i_remove idx tbl =
  { ilist = List.remove_assoc idx tbl.ilist; klist = tbl.klist }

let k_remove key tbl =
  { ilist = tbl.ilist; klist = List.remove_assoc key tbl.klist }

let remove get_int_key_opt key tbl =
  match get_int_key_opt key with
  | Some idx -> i_remove idx tbl
  | None -> k_remove key tbl

let i_get idx tbl = List.assoc_opt idx tbl.ilist

let k_get key tbl = List.assoc_opt key tbl.klist

let get get_int_key_opt key tbl =
  match get_int_key_opt key with
  | Some idx -> i_get idx tbl
  | None -> k_get key tbl

(* len: "border" concept https://www.lua.org/manual/5.4/manual.html#3.4.7
    Here: number of contiguous elements from 1
    this is not the exact specification
*)
let len tbl =
  let rec cpt idx itbl acc_len =
    match List.mem_assoc idx itbl with
    | false -> acc_len
    | true -> cpt (idx + 1) itbl (acc_len + 1)
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
  match key with
  | Ikey idx -> begin
    match tbl.ilist with
    | [] -> assert false (* PC2 *)
    | (i, _) :: tl when idx = i -> first_elt { ilist = tl; klist = tbl.klist }
    | _ :: tl -> next_elt (Ikey idx) { ilist = tl; klist = tbl.klist }
  end
  | Kkey key -> (
    match tbl.klist with
    | [] -> assert false (* PC2 *)
    | (k, _) :: tl when key = k -> first_k_elt { ilist = tbl.ilist; klist = tl }
    | _ :: tl -> next_elt (Kkey key) { ilist = tbl.ilist; klist = tl } )

(* https://www.lua.org/manual/5.4/manual.html#6.1
   TODO: Warning spec not fully implemented *)

let next key tbl =
  (* first level checks : empty table and key existence *)
  match is_empty tbl with
  | true -> None
  | false -> (
    match key with
    | Some (Ikey idx) -> begin
      match i_get idx tbl with
      | Some _ -> next_elt (Ikey idx) tbl
      | None -> error ("invalid key to 'next': " ^ Int.to_string idx)
    end
    | Some (Kkey key) -> begin
      match k_get key tbl with
      | Some _ -> next_elt (Kkey key) tbl
      | None -> error "invalid key to 'next'"
    end
    | None -> first_elt tbl )
