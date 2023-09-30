(* Typer *)

open Ast

exception Typing_error of string

let error message = raise (Typing_error message)

let rec typecheck_expr expr =
  let typecheck_value value =
    match value with
    | Vnil _ -> Tnil
    | Vboolean _ -> Tboolean
    | Vnumber (Ninteger _) -> Tnumber Tinteger
    | Vnumber (Nfloat _) -> Tnumber Tfloat
    | Vstring _ -> Tstring
  in
  let typecheck_arith_unop expr =
    let typ = typecheck_expr expr in
    match typ with
    | Tnumber Tinteger -> Tnumber Tinteger
    | Tnumber Tfloat -> Tnumber Tfloat
    | Tnil -> error "attempt to perform arithmetic on a nil value"
    | Tboolean -> error "attempt to perform arithmetic on a boolean value"
    | Tstring -> error "attempt to perform arithmetic on a string value"
    | _ -> assert false (* call error *)
  in
  let typecheck_arith_binop binop expr1 expr2 =
    let typ1 = typecheck_expr expr1 in
    let typ2 = typecheck_expr expr2 in
    match (typ1, typ2) with
    | Tnumber Tinteger, Tnumber Tinteger ->
      if binop = Bdiv || binop = Bexp then Tnumber Tfloat else Tnumber Tinteger
    | Tnumber Tfloat, Tnumber Tfloat
    | Tnumber Tinteger, Tnumber Tfloat
    | Tnumber Tfloat, Tnumber Tinteger ->
      Tnumber Tfloat
    | Tnil, _ | _, Tnil -> error "attempt to perform arithmetic on a nil value"
    | Tboolean, _ | _, Tboolean ->
      error "attempt to perform arithmetic on a boolean value"
    | Tstring, _ | _, Tstring ->
      error "attempt to perform arithmetic on a string value"
    | _ -> assert false (* call error *)
  in
  let typecheck_str_binop expr1 expr2 =
    let typ1 = typecheck_expr expr1 in
    let typ2 = typecheck_expr expr2 in
    match (typ1, typ2) with
    | Tstring, Tstring
    | Tstring, Tnumber Tinteger
    | Tstring, Tnumber Tfloat
    | Tnumber Tinteger, Tstring
    | Tnumber Tfloat, Tstring
    | Tnumber Tinteger, Tnumber Tinteger
    | Tnumber Tfloat, Tnumber Tfloat
    | Tnumber Tinteger, Tnumber Tfloat
    | Tnumber Tfloat, Tnumber Tinteger ->
      Tstring
    | Tnil, _ | _, Tnil -> error "attempt to concatenate a nil value"
    | Tboolean, _ | _, Tboolean ->
      error "attempt to concatenate a boolean value"
    | _ -> assert false (* call error *)
  in
  match expr with
  | Evalue v -> typecheck_value v
  | Eident _i -> Tnil (* TODO *)
  | Eunop (Unot, _) -> Tboolean
  | Eunop (Uminus, e) -> typecheck_arith_unop e
  | Eunop (Usharp, e) ->
    let typ = typecheck_expr e in
    begin
      match typ with
      | Tstring -> Tnumber Tinteger
      | Tnil | Tboolean | Tnumber _ ->
        error "attempt to perform #operator on a not supported type"
      | _ -> error "#operator on this type: to be implemented ..."
    end
  | Ebinop (Band, _, _) | Ebinop (Bor, _, _) -> Tboolean (* TODO *)
  | Ebinop (Badd, e1, e2) -> typecheck_arith_binop Badd e1 e2
  | Ebinop (Bsub, e1, e2) -> typecheck_arith_binop Bsub e1 e2
  | Ebinop (Bmul, e1, e2) -> typecheck_arith_binop Bmul e1 e2
  | Ebinop (Bdiv, e1, e2) -> typecheck_arith_binop Bdiv e1 e2
  | Ebinop (Bfldiv, e1, e2) -> typecheck_arith_binop Bfldiv e1 e2
  | Ebinop (Bmod, e1, e2) -> typecheck_arith_binop Bmod e1 e2
  | Ebinop (Bexp, e1, e2) -> typecheck_arith_binop Bexp e1 e2
  | Ebinop (Blt, _, _)
  | Ebinop (Ble, _, _)
  | Ebinop (Bgt, _, _)
  | Ebinop (Bge, _, _)
  | Ebinop (Beq, _, _)
  | Ebinop (Bneq, _, _) ->
    Tboolean (* TODO *)
  | Ebinop (Bddot, e1, e2) -> typecheck_str_binop e1 e2

let rec typecheck_stmt stmt =
  match stmt with
  | Sempty -> ()
  | Sblock b -> typecheck_block b
  | Swhile (_e, b) -> typecheck_block b
  | Sprint e ->
    let _ = typecheck_expr e in
    ()

and typecheck_block b = List.iter typecheck_stmt b

let typecheck chunk = typecheck_block chunk
