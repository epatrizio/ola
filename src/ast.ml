(* Abstract Syntax Tree *)

type typ =
  | Tnil
  | Tnumber
  | Tboolean
  | Tstring
  | Tfunction
  | Tuserdata
  | Tthread
  | Ttable

type ident = string

(* type unop =
  | Unot not e *)

(* type binop =
  | Badd
  | Bsub | Bmul | Bdiv           (* + - * / *)
  | Beq | Bneq | Blt | Ble | Bgt | Bge  (* == != < <= > >= *)
  | Band | Bor                          && || *)

type number =
  | Ninteger of int
  | Nfloat of float

type value =
  | Vnil
  | Vfalse of bool
  | Vtrue of bool
  | Vnumber of number
  | Vstring of string

type expr =
  | Evalue of value
  | Eident of ident
  (* | Eunop of unop * expr
  | Ebinop of binop * expr * expr *)

type stmt =
  | Sblock of block
  (* | Sassign of var list * expr list *)
  | Sprint of expr

and block =
  | Bstmt of stmt list

type script = stmt
