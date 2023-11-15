(* open Ast *)

(* MEMO: compare
   All functions ignore non-numeric keys in the tables given as arguments.
*)

type ('a, 'b) t = ('a * 'b) list

let empty = []

let rec add key value tbl =
  match tbl with
  | [] -> [ (key, value) ]
  | (k, _v) :: tl when k = key -> (k, value) :: tl
  | (k, v) :: tl -> (k, v) :: add key value tl

let remove key tbl = List.remove_assoc key tbl

let get key tbl = List.assoc_opt key tbl

let len tbl = List.length tbl
