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

let rec get key tbl =
  match tbl with
  | [] -> None
  | (k, v) :: _tl when k = key -> Some v
  | (_k, _v) :: tl -> get key tl

let len tbl = List.length tbl
