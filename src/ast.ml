(* Abstract Syntax Tree *)

type location = Lexing.position * Lexing.position

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

type name = string

type attrib = name

type funcname = name (* * name list option * name option *)

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
  | Vfunction of int32 * funcbody (* int32 = function unique id *)

and expr = location * expr'

and expr' =
  | Evalue of value
  | Eunop of unop * expr
  | Ebinop of binop * expr * expr
  | Evariadic
  | Efunctiondef of funcbody
  | Eprefix of prefixexp

and prefixexp =
  | PEvar of var
  | PEfunctioncall of functioncall
  | PEexp of expr

(* var ::=  Name | prefixexp ‘[’ exp ‘]’ | prefixexp ‘.’ Name  *)
and var = Name of name

and functioncall = FCpreargs of expr * args
(* | FCprename of prefixexp * name * args *)

and funcbody = parlist * block

and parlist =
  | PLlist of name list * expr option
  | PLvariadic of expr

(* args ::= ‘(’ [explist] ‘)’ | tableconstructor | LiteralString  *)
and args = Aexplist of expr list

and stmt =
  | Sempty
  | Sassign of var list * expr list
  | SassignLocal of (name * attrib option) list * expr list option
  | Sbreak
  | Sreturn of expr list option * stmt option
  | Slabel of name
  | Sgoto of name
  | Sblock of block
  | Swhile of expr * block
  | Srepeat of block * expr
  | Sif of expr * block * (expr * block) list * block option
  | Sfor of name * expr * expr * expr option * block
  | Siterator of name list * expr list * block
  (* | Sfunction of funcname * funcbody *)
  (* transform Sassign *)
  (* | SfunctionLocal of name * funcbody *)
  (* transform SassignLocal *)
  | SfunctionCall of functioncall
  | Sprint of expr

and block = stmt list

type chunk = block

(* pretty printer *)

let pp_sep fmt () = Format.fprintf fmt ", "

let to_el = List.map (fun (_, e) -> e)

let print_funcname fmt funcname = Format.fprintf fmt "%s" funcname

let print_attrib fmt attrib = Format.fprintf fmt "<%s>" attrib

let print_var fmt var = match var with Name n -> Format.pp_print_string fmt n

let print_unop fmt unop =
  match unop with
  | Unot -> Format.pp_print_string fmt "not "
  | Uminus -> Format.pp_print_string fmt "-"
  | Usharp -> Format.pp_print_string fmt "#"
  | Ulnot -> Format.pp_print_string fmt "~"

let print_binop fmt binop =
  match binop with
  | Band -> Format.pp_print_string fmt " and "
  | Bor -> Format.pp_print_string fmt " or "
  | Badd -> Format.pp_print_string fmt "+"
  | Bsub -> Format.pp_print_string fmt "-"
  | Bmul -> Format.pp_print_string fmt "*"
  | Bdiv -> Format.pp_print_string fmt "/"
  | Bfldiv -> Format.pp_print_string fmt "//"
  | Bmod -> Format.pp_print_string fmt "%"
  | Bexp -> Format.pp_print_string fmt "^"
  | Bland -> Format.pp_print_string fmt "&"
  | Blor -> Format.pp_print_string fmt "|"
  | Blxor -> Format.pp_print_string fmt "~"
  | Blsl -> Format.pp_print_string fmt "<<"
  | Blsr -> Format.pp_print_string fmt ">>"
  | Blt -> Format.pp_print_string fmt "<"
  | Ble -> Format.pp_print_string fmt "<="
  | Bgt -> Format.pp_print_string fmt ">"
  | Bge -> Format.pp_print_string fmt ">="
  | Beq -> Format.pp_print_string fmt "=="
  | Bneq -> Format.pp_print_string fmt "~="
  | Bddot -> Format.pp_print_string fmt ".."

let print_number fmt number =
  match number with
  | Ninteger i -> Format.pp_print_int fmt i
  | Nfloat f -> Format.pp_print_float fmt f

let print_value fmt value =
  match value with
  | Vnil () -> Format.pp_print_string fmt "nil"
  | Vboolean b -> Format.pp_print_bool fmt b
  | Vnumber num -> print_number fmt num
  | Vstring s -> Format.fprintf fmt "\"%a\"" Format.pp_print_string s
  | Vfunction (i, _b) ->
    Format.fprintf fmt "function: %a" Format.pp_print_int (Int32.to_int i)

let rec print_parlist fmt pl =
  match pl with
  | PLlist (nl, eo) ->
    Format.fprintf fmt "%a%a"
      (Format.pp_print_list ~pp_sep Format.pp_print_string)
      nl print_eo eo
  | PLvariadic e ->
    let _, e = e in
    print_expr fmt e

and print_eo fmt eo =
  match eo with
  | Some (_, e) -> Format.fprintf fmt ", %a" print_expr e
  | None -> ()

and print_funcbody fmt fb =
  let pl, b = fb in
  Format.fprintf fmt "(%a)@.%a@.end@." print_parlist pl print_block b

and print_args fmt args =
  match args with
  | Aexplist el ->
    Format.fprintf fmt "(%a)"
      (Format.pp_print_list ~pp_sep print_expr)
      (to_el el)

and print_prefixexp fmt prexp =
  match prexp with
  | PEvar v -> print_var fmt v
  | PEfunctioncall fc -> print_functioncall fmt fc
  | PEexp e ->
    let _, e = e in
    print_expr fmt e

and print_functioncall fmt fc =
  match fc with
  | FCpreargs (e, args) ->
    let _, e = e in
    Format.fprintf fmt "%a%a" print_expr e print_args args

and print_expr fmt expr =
  match expr with
  | Evalue v -> print_value fmt v
  | Eunop (uop, (_, e)) ->
    print_unop fmt uop;
    print_expr fmt e
  | Ebinop (bop, (_, e1), (_, e2)) ->
    print_expr fmt e1;
    print_binop fmt bop;
    print_expr fmt e2
  | Evariadic -> Format.pp_print_string fmt "..."
  | Efunctiondef fb -> Format.fprintf fmt "function %a" print_funcbody fb
  | Eprefix prexp -> print_prefixexp fmt prexp

and print_stmt fmt stmt =
  let pp_name_attrib fmt (name, attrib_opt) =
    Format.fprintf fmt "%s" name;
    match attrib_opt with
    | Some attrib -> Format.fprintf fmt " %a " print_attrib attrib
    | None -> ()
  in
  let pp_exprlist_opt fmt exprlist_opt =
    match exprlist_opt with
    | Some exprlist ->
      Format.fprintf fmt " = %a"
        (Format.pp_print_list ~pp_sep print_expr)
        (to_el exprlist)
    | None -> ()
  in
  let pp_stmt_opt fmt stmt_opt =
    match stmt_opt with Some stmt -> print_stmt fmt stmt | None -> ()
  in
  match stmt with
  | Sempty -> Format.fprintf fmt ""
  | Sassign (il, lel) ->
    Format.fprintf fmt "%a = %a@."
      (Format.pp_print_list ~pp_sep print_var)
      il
      (Format.pp_print_list ~pp_sep print_expr)
      (to_el lel)
  | SassignLocal (nal, elo) ->
    Format.fprintf fmt "local %a%a@."
      (Format.pp_print_list ~pp_sep pp_name_attrib)
      nal pp_exprlist_opt elo
  | Sbreak -> Format.fprintf fmt "break@."
  | Sreturn (elo, so) ->
    Format.fprintf fmt "return %a%a@." pp_exprlist_opt elo pp_stmt_opt so
  | Slabel n -> Format.fprintf fmt "::%s::@." n
  | Sgoto n -> Format.fprintf fmt "goto %s@." n
  | Sblock b -> Format.fprintf fmt "do@.@[<v>%a@]end@." print_block b
  | Swhile ((_, e), b) ->
    Format.fprintf fmt "while %a do@.@[<v>%a@]end@." print_expr e print_block b
  | Srepeat (b, (_, e)) ->
    Format.fprintf fmt "repeat@.@[<v>%a@]until %a@." print_block b print_expr e
  | Sif ((_, e), b, lebl, ob) ->
    let to_ebl = List.map (fun ((_, e), b) -> (e, b)) in
    let print_elseif fmt (e, b) =
      Format.fprintf fmt "elseif %a then %a@." print_expr e print_block b
    in
    Format.fprintf fmt "if %a then %a %a" print_expr e print_block b
      (Format.pp_print_list print_elseif)
      (to_ebl lebl);
    begin
      match ob with
      | Some b -> Format.fprintf fmt "else %a end@." print_block b
      | None -> Format.fprintf fmt "end@."
    end
  | Sfor (n, (_, e1), (_, e2), oe, b) ->
    Format.fprintf fmt "for %a = %a, %a%a do@.@[<v>%a@]end@."
      Format.pp_print_string n print_expr e1 print_expr e2 print_eo oe
      print_block b
  | Siterator (nl, lel, b) ->
    Format.fprintf fmt "for %a = %a do@.@[<v>%a@]end@."
      (Format.pp_print_list ~pp_sep Format.pp_print_string)
      nl
      (Format.pp_print_list ~pp_sep print_expr)
      (to_el lel) print_block b
  (* | Sfunction (fname, fbody) ->
     Format.fprintf fmt "function %a%a" print_funcname fname print_funcbody fbody *)
  (* | SfunctionLocal (name, fbody) ->
     Format.fprintf fmt "local function %a%a" print_var (Name name)
       print_funcbody fbody *)
  | SfunctionCall fc -> print_functioncall fmt fc
  | Sprint (_, e) -> Format.fprintf fmt "print(%a)@." print_expr e

and print_block fmt block =
  let pp_sep _fmt () = () in
  Format.pp_print_list ~pp_sep print_stmt fmt block

let print_chunk fmt chunk = print_block fmt chunk
