(* open Ast *)

(* MEMO: compare
   All functions ignore non-numeric keys in the tables given as arguments.
*)

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
