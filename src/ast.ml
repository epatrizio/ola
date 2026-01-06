(* Abstract Syntax Tree *)

type location = Lexing.position * Lexing.position

let empty_location () : location =
  ( { Lexing.pos_fname = ""
    ; Lexing.pos_lnum = 0
    ; Lexing.pos_bol = 0
    ; Lexing.pos_cnum = 0
    }
  , { Lexing.pos_fname = ""
    ; Lexing.pos_lnum = 0
    ; Lexing.pos_bol = 0
    ; Lexing.pos_cnum = 0
    } )

type number_type =
  | Tinteger
  | Tfloat

type typ =
  | Tnil
  | Tboolean
  | TnumberUndefined
  | Tnumber of number_type
  | Tstring
  | Tvariadic of typ list
  | Tfunction
  | TfunctionStdLib
  | TfunctionReturn of typ list
  | Ttable
  | Tuserdata
  | Tthread

type unop =
  | Unot
  | Uminus
  | Usharp
  | Ulnot

type binop =
  | Band
  | Bor
  | Badd
  | Bsub
  | Bmul
  | Bdiv
  | Bfldiv
  | Bmod
  | Bexp
  | Bland
  | Blor
  | Blxor
  | Blsl
  | Blsr
  | Beq
  | Bneq
  | Blt
  | Ble
  | Bgt
  | Bge
  | Bddot

type number =
  | Ninteger of int
  | Nfloat of float

type value =
  | Vnil of unit
  | Vboolean of bool
  | Vnumber of number
  | Vstring of string
  | Vvariadic of value list
  | Vfunction of int32 * (parlist * block) * value Env.t
    (* int32 = function unique id *)
  | VfunctionStdLib of
      int32 * (value list -> value Env.t -> value list * value Env.t)
    (* int32 = stdlib function unique id *)
  | VfunctionReturn of value list
  | Vtable of int32 * (value, value) Table.t (* int32 = table unique id *)

and expr = location * expr'

and expr' =
  | Evalue of value
  | Eunop of unop * expr
  | Ebinop of expr * binop * expr
  | Evariadic
  | Efunctiondef of (parlist * block)
  | Eprefix of prefixexp
  | Etableconstructor of field list

and prefixexp =
  | PEvar of var
  | PEfunctioncall of functioncall
  | PEexp of expr

and var =
  | VarName of string
  | VarTableField of prefixexp * expr
(* | VarTableFieldName of prefixexp * string *)
(* syntactic sugar: transform VarTableField in parser *)

and args = Aexpl of expr list
(* | Atable of field list *)
(* syntactic sugar: transform Atable in parser *)
(* | Astr of string *)
(* syntactic sugar: transform Aexpl in parser *)

and functioncall =
  | FCpreargs of prefixexp * args
  | FCprename of prefixexp * string * args

and parlist =
  | PLlist of string list * bool
  | PLvariadic

and field =
  | Fexp of expr
  | Fname of string * expr
  | Fcol of expr * expr

and stmt =
  | Sempty
  | Sassign of var list * expr list
  | SassignLocal of (string * string option) list * expr list
  | Sbreak
  | Sreturn of expr list
  | Slabel of string
  | Sgoto of string
  | Sblock of block
  | Swhile of expr * block
  | Srepeat of block * expr
  | Sif of expr * block * (expr * block) list * block option
  | Sfor of string * expr * expr * expr option * block
  | Siterator of string list * expr list * block
  (* | Sfunction of funcname * funcbody *)
  (* syntactic sugar: transform Sassign in parser *)
  (* | SfunctionLocal of name * funcbody *)
  (* syntactic sugar: transform SassignLocal in parser *)
  | SfunctionCall of functioncall

and block = stmt list

(* pretty printer *)

open Format

let pp_sep fmt () = fprintf fmt ", "

let print_attrib fmt attrib = fprintf fmt {|<%s>|} attrib

let print_unop fmt unop =
  pp_print_string fmt
  @@
  match unop with
  | Unot -> "not "
  | Uminus -> "-"
  | Usharp -> "#"
  | Ulnot -> "~"

let print_binop fmt binop =
  pp_print_string fmt
  @@
  match binop with
  | Band -> " and "
  | Bor -> " or "
  | Badd -> "+"
  | Bsub -> "-"
  | Bmul -> "*"
  | Bdiv -> "/"
  | Bfldiv -> "//"
  | Bmod -> "%"
  | Bexp -> "^"
  | Bland -> "&"
  | Blor -> "|"
  | Blxor -> "~"
  | Blsl -> "<<"
  | Blsr -> ">>"
  | Blt -> "<"
  | Ble -> "<="
  | Bgt -> ">"
  | Bge -> ">="
  | Beq -> "=="
  | Bneq -> "~="
  | Bddot -> ".."

let print_number fmt number =
  match number with
  | Ninteger i -> pp_print_int fmt i
  | Nfloat f -> pp_print_float fmt f

let rec print_value fmt value =
  match value with
  | Vnil () -> pp_print_string fmt "nil"
  | Vboolean b -> pp_print_bool fmt b
  | Vnumber num -> print_number fmt num
  | Vstring s -> fprintf fmt {|"%a"|} pp_print_string s
  | Vfunction (i, _, _) | VfunctionStdLib (i, _) ->
    fprintf fmt {|function: %a|} pp_print_int (Int32.to_int i)
  | VfunctionReturn vl | Vvariadic vl ->
    (pp_print_list ~pp_sep print_value) fmt vl
  | Vtable (i, _flo) -> fprintf fmt {|table: %a|} pp_print_int (Int32.to_int i)

let rec print_parlist fmt pl =
  match pl with
  | PLlist (nl, true) ->
    fprintf fmt {|%a, ...|} (pp_print_list ~pp_sep pp_print_string) nl
  | PLlist (nl, false) ->
    fprintf fmt {|%a|} (pp_print_list ~pp_sep pp_print_string) nl
  | PLvariadic -> fprintf fmt "..."

and print_var fmt v =
  match v with
  | VarName n -> pp_print_string fmt n
  | VarTableField (pexp, exp) ->
    fprintf fmt {|%a[%a]|} print_prefixexp pexp print_expr exp
(* | VarTableFieldName (pexp, n) -> fprintf fmt {|%a.%s|} print_prefixexp pexp n *)

and print_field fmt f =
  match f with
  | Fexp e -> print_expr fmt e
  | Fname (n, e) -> fprintf fmt {|%s = %a|} n print_expr e
  | Fcol (e1, e2) -> fprintf fmt {|[%a] = %a|} print_expr e1 print_expr e2

and print_fieldlist fmt fl =
  fprintf fmt {|{%a}|} (pp_print_list ~pp_sep print_field) fl

and print_eo fmt eo = Option.iter (fprintf fmt {|, %a|} print_expr) eo

and print_funcbody fmt (pl, b) =
  fprintf fmt {|(%a)@,%aend|} print_parlist pl print_block b

and print_prefixexp fmt prexp =
  match prexp with
  | PEvar v -> print_var fmt v
  | PEfunctioncall fc -> print_functioncall fmt fc
  | PEexp e -> fprintf fmt {|(%a)|} print_expr e

and print_args fmt args =
  match args with
  | Aexpl el -> fprintf fmt {|(%a)|} (pp_print_list ~pp_sep print_expr) el
(* | Atable fl -> print_fieldlist fmt fl *)
(* | Astr s -> pp_print_string fmt s *)

and print_functioncall fmt fc =
  match fc with
  | FCpreargs (pexp, args) ->
    fprintf fmt {|%a%a|} print_prefixexp pexp print_args args
  | FCprename (pexp, n, args) ->
    fprintf fmt {|%a:%s(%a)|} print_prefixexp pexp n print_args args

and print_expr fmt (_loc, expr) =
  match expr with
  | Evalue v -> print_value fmt v
  | Eunop (uop, e) ->
    print_unop fmt uop;
    print_expr fmt e
  | Ebinop (e1, bop, e2) ->
    print_expr fmt e1;
    print_binop fmt bop;
    print_expr fmt e2
  | Evariadic -> pp_print_string fmt "..."
  | Efunctiondef fb -> fprintf fmt {|function %a|} print_funcbody fb
  | Eprefix prexp -> print_prefixexp fmt prexp
  | Etableconstructor fl -> print_fieldlist fmt fl

and print_stmt fmt stmt =
  let pp_name_attrib fmt (name, attrib_opt) =
    pp_print_string fmt name;
    Option.iter (fprintf fmt {| %a |} print_attrib) attrib_opt
  in
  match stmt with
  | Sempty -> fprintf fmt ""
  | Sassign (vl, lel) ->
    fprintf fmt {|%a = %a@,|}
      (pp_print_list ~pp_sep print_var)
      vl
      (pp_print_list ~pp_sep print_expr)
      lel
  | SassignLocal (nal, el) ->
    fprintf fmt {|local %a%a%a@,|}
      (pp_print_list ~pp_sep pp_name_attrib)
      nal pp_print_string
      (if List.length el > 0 then " = " else "")
      (pp_print_list ~pp_sep print_expr)
      el
  | Sbreak -> fprintf fmt {|break@,|}
  | Sreturn el ->
    fprintf fmt {|return %a@,|} (pp_print_list ~pp_sep print_expr) el
  | Slabel n -> fprintf fmt {|::%s::@,|} n
  | Sgoto n -> fprintf fmt {|goto %s@,|} n
  | Sblock b -> fprintf fmt {|do@,%aend@,|} print_block b
  | Swhile (e, b) ->
    fprintf fmt {|while %a do@,%aend@,|} print_expr e print_block b
  | Srepeat (b, e) ->
    fprintf fmt {|repeat@,%auntil %a@,|} print_block b print_expr e
  | Sif (e, b, lebl, ob) ->
    let print_elseif fmt (e, b) =
      fprintf fmt {|elseif %a then@,%a|} print_expr e print_block b
    in
    fprintf fmt {|if %a then@,%a%a|} print_expr e print_block b
      (pp_print_list print_elseif)
      lebl;
    Option.iter (fprintf fmt {|else@,%a|} print_block) ob;
    fprintf fmt "end@,"
  | Sfor (n, e1, e2, oe, b) ->
    fprintf fmt {|for %a = %a, %a%a do@,%aend@,|} pp_print_string n print_expr
      e1 print_expr e2 print_eo oe print_block b
  | Siterator (nl, lel, b) ->
    fprintf fmt {|for %a = %a do@,%aend@,|}
      (pp_print_list ~pp_sep pp_print_string)
      nl
      (pp_print_list ~pp_sep print_expr)
      lel print_block b
  (* | Sfunction (fname, fbody) ->
     fprintf fmt {|function %a%a|} print_funcname fname print_funcbody fbody *)
  (* | SfunctionLocal (name, fbody) ->
     fprintf fmt {|local function %a%a|} print_var (Name name)
       print_funcbody fbody *)
  | SfunctionCall fc -> print_functioncall fmt fc

and print_block fmt block =
  let pp_sep fmt () = fprintf fmt "@," in
  fprintf fmt {|@[<v 2>%a@]|} (pp_print_list ~pp_sep print_stmt) block

let pp_loc fmt (loc : location) =
  let start, _end = loc in
  let file = start.pos_fname in
  let line = start.pos_lnum in
  let char = start.pos_cnum - start.pos_bol in
  Format.fprintf fmt {|File "%s", line %d, char %d|} file line char
