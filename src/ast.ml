(* Abstract Syntax Tree *)

type number_type =
  | Tinteger
  | Tfloat

type typ =
  | Tnil
  | Tboolean
  | Tnumber of number_type
  | Tstring
  | Tfunction
  | Tuserdata
  | Tthread
  | Ttable

type number =
  | Ninteger of int
  | Nfloat of float

type value =
  | Vnil of unit
  | Vboolean of bool
  | Vnumber of number
  | Vstring of string

type ident = string

type unop =
  | Unot
  | Uminus

type binop =
  | Band | Bor
  | Badd | Bsub | Bmul
(*  | Bdiv           (* + - * / *)
  | Beq | Bneq | Blt | Ble | Bgt | Bge  (* == != < <= > >= *) *)

type typed_value = {
  typ : typ;
  value : value
}

type expr =
  | Evalue of value
  | Eident of ident
  | Eunop of unop * expr
  | Ebinop of binop * expr * expr

type stmt =
  | Sblock of block
  (* | Sassign of var list * expr list *)
  | Sprint of expr

and block =
  | Bstmt of stmt list

type script = stmt list

(* pretty printer *)

let print_ident fmt ident =
  match ident with
  | ident -> Format.pp_print_string fmt ident 

let print_unop fmt unop =
  match unop with
  | Unot -> Format.pp_print_string fmt "not "
  | Uminus -> Format.pp_print_string fmt "-"

let print_binop fmt binop =
  match binop with
  | Band -> Format.pp_print_string fmt " and "
  | Bor -> Format.pp_print_string fmt " or "
  | Badd -> Format.pp_print_string fmt "+"
  | Bsub -> Format.pp_print_string fmt "-"
  | Bmul -> Format.pp_print_string fmt "*"

let print_number fmt number =
  match number with
  | Ninteger i -> Format.pp_print_int fmt i
  | Nfloat f -> Format.pp_print_float fmt f

let print_value fmt value =
  match value with
  | Vnil _ -> Format.pp_print_string fmt "nil"
  | Vboolean b -> Format.pp_print_bool fmt b
  | Vnumber num -> print_number fmt num
  | Vstring s -> Format.pp_print_string fmt s

let print_typed_value fmt typed_value =
  let {typ = _; value = v} = typed_value in
  print_value fmt v

let rec print_expr fmt expr =
  match expr with
  | Evalue v -> print_value fmt v
  | Eident i -> print_ident fmt i
  | Eunop (uop, e) ->
    print_unop fmt uop;
    print_expr fmt e
  | Ebinop (bop, e1, e2) ->
    print_expr fmt e1;
    print_binop fmt bop;
    print_expr fmt e2

let print_stmt fmt stmt =
  match stmt with
  | Sblock _b -> print_endline "not implemented"
  | Sprint e ->
    Format.fprintf fmt "print(";
    print_expr fmt e;
    Format.fprintf fmt ")@."

let print_script fmt script =
  List.iter (print_stmt fmt) script
