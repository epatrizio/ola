(* Typer *)

(* Note :
   Now typecheck is performed during interpretation on each expressions.
   So, statement typecheck is no longer used, except Sfor for init values.
   See PR https://github.com/epatrizio/ola/pull/44
   - typecheck_stmt and typecheck_block functions removal
   - typecheck_for_ctrl_expr and typecheck_iterator_ctrl_el new functions for Sfor et Siterator old stmts
*)

open Ast
open Ast.Value
open Syntax

exception Typing_error of Ast.location option * string

let error loc_opt message = raise (Typing_error (loc_opt, message))

let rec typecheck_value value env =
  match value with
  | Vnil () -> Tnil
  | Vboolean _ -> Tboolean
  | Vnumber (Ninteger _) -> Tnumber Tinteger
  | Vnumber (Nfloat _) -> Tnumber Tfloat
  | Vstring _ -> Tstring
  | Vvariadic vl -> Tvariadic (List.map (fun v -> typecheck_value v env) vl)
  | Vfunction _ -> Tfunction
  | VfunctionReturn vl ->
    TfunctionReturn (List.map (fun v -> typecheck_value v env) vl)
  | VfunctionStdLib _ -> TfunctionStdLib
  | Vtable _ -> Ttable
  | Vref v -> (
    match typecheck_var v env with
    | Ok typ -> Tref typ
    | Error (loc_opt, msg) -> error loc_opt msg )

and typecheck_function value =
  match value with
  | Vfunction _ -> Ok Tfunction
  | VfunctionStdLib _ -> Ok TfunctionStdLib
  | VfunctionReturn [] -> error None "attempt to call a nil value"
  | VfunctionReturn (v :: _) -> typecheck_function v
  | Vnil _ -> error None "attempt to call a nil value"
  | _ -> error None "attempt to call a non function value"

and typecheck_variadic value env =
  let typ = typecheck_value value env in
  match typ with Tvariadic _ -> Ok typ | _ -> assert false

and typecheck_arith_unop ((loc, _e) as expr) env =
  let* t = typecheck_expr expr env in
  match t with
  | Tnumber Tinteger -> Ok (Tnumber Tinteger)
  | Tnumber Tfloat -> Ok (Tnumber Tfloat)
  | Tnil -> error (Some loc) "attempt to perform arithmetic on a nil value"
  | Tboolean ->
    error (Some loc) "attempt to perform arithmetic on a boolean value"
  | Tfunction ->
    error (Some loc) "attempt to perform arithmetic on a function value"
  | Ttable -> error (Some loc) "attempt to perform arithmetic on a table value"
  | _ -> assert false (* call error *)

and typecheck_sharp_unop ((loc, _e) as expr) env =
  let* t = typecheck_expr expr env in
  match t with
  | Tstring -> Ok (Tnumber Tinteger)
  | Ttable -> Ok (Tnumber Tinteger)
  | Tnil -> error (Some loc) "attempt to get length of a nil value"
  | Tboolean -> error (Some loc) "attempt to get length of a boolean value"
  | Tnumber _ -> error (Some loc) "attempt to get length of a number value"
  | Tfunction -> error (Some loc) "attempt to get length of a function value"
  | _ -> assert false (* call error *)

and typecheck_bitwise_unop ((loc, _e) as expr) env =
  let* t = typecheck_expr expr env in
  match t with
  | Tnumber Tinteger -> Ok (Tnumber Tinteger)
  | Tnil ->
    error (Some loc) "attempt to perform bitwise operation on a nil value"
  | Tboolean ->
    error (Some loc) "attempt to perform bitwise operation on a boolean value"
  | Tstring ->
    error (Some loc) "attempt to perform bitwise operation on a string value"
  | Tfunction ->
    error (Some loc) "attempt to perform bitwise operation on a function value"
  | Ttable ->
    error (Some loc) "attempt to perform bitwise operation on a table value"
  | _ -> assert false (* call error *)

and typecheck_arith_binop binop ((loc1, _e1) as expr1) ((loc2, _e2) as expr2)
  env =
  let* typ1 = typecheck_expr expr1 env in
  let* typ2 = typecheck_expr expr2 env in
  match (typ1, typ2) with
  | Tnumber Tinteger, Tnumber Tinteger ->
    Ok
      (if binop = Bdiv || binop = Bexp then Tnumber Tfloat else Tnumber Tinteger)
  | Tnumber _, Tnumber _ -> Ok (Tnumber Tfloat)
  | Tnil, _ -> error (Some loc1) "attempt to perform arithmetic on a nil value"
  | _, Tnil -> error (Some loc2) "attempt to perform arithmetic on a nil value"
  | Tboolean, _ ->
    error (Some loc1) "attempt to perform arithmetic on a boolean value"
  | _, Tboolean ->
    error (Some loc2) "attempt to perform arithmetic on a boolean value"
  | _ -> assert false (* call error *)

and typecheck_bitwise_binop ((loc1, _e1) as expr1) ((loc2, _e2) as expr2) env =
  let* typ1 = typecheck_expr expr1 env in
  let* typ2 = typecheck_expr expr2 env in
  match (typ1, typ2) with
  | Tnumber Tinteger, Tnumber Tinteger -> Ok (Tnumber Tinteger)
  | Tnil, _ ->
    error (Some loc1) "attempt to perform bitwise operation on a nil value"
  | _, Tnil ->
    error (Some loc2) "attempt to perform bitwise operation on a nil value"
  | Tboolean, _ ->
    error (Some loc1) "attempt to perform bitwise operation on a boolean value"
  | _, Tboolean ->
    error (Some loc2) "attempt to perform bitwise operation on a boolean value"
  | Tstring, _ ->
    error (Some loc1) "attempt to perform bitwise operation on a string value"
  | _, Tstring ->
    error (Some loc2) "attempt to perform bitwise operation on a string value"
  | _ -> assert false (* call error *)

and typecheck_rel_binop binop ((loc1, _e1) as expr1) ((_loc2, _e2) as expr2) env
    =
  let* typ1 = typecheck_expr expr1 env in
  let* typ2 = typecheck_expr expr2 env in
  match (typ1, typ2) with
  | Tnumber _, Tnumber _ | Tstring, Tstring -> Ok Tboolean
  | _, _ -> (
    match binop with
    | Beq | Bneq -> Ok Tboolean
    | _ ->
      error (Some loc1) "attempt to compare two values with non correct types" )

and typecheck_str_binop ((loc1, _e1) as expr1) ((loc2, _e2) as expr2) env =
  let* typ1 = typecheck_expr expr1 env in
  let* typ2 = typecheck_expr expr2 env in
  match (typ1, typ2) with
  | (Tstring | Tnumber _), (Tstring | Tnumber _) -> Ok Tstring
  | Tnil, _ -> error (Some loc1) "attempt to concatenate a nil value"
  | _, Tnil -> error (Some loc2) "attempt to concatenate a nil value"
  | Tboolean, _ -> error (Some loc1) "attempt to concatenate a boolean value"
  | _, Tboolean -> error (Some loc2) "attempt to concatenate a boolean value"
  | Tfunction, _ -> error (Some loc1) "attempt to concatenate a function value"
  | _, Tfunction -> error (Some loc2) "attempt to concatenate a function value"
  | Ttable, _ -> error (Some loc1) "attempt to concatenate a table value"
  | _, Ttable -> error (Some loc2) "attempt to concatenate a table value"
  | _ -> assert false (* call error *)

and typecheck_var ?(strict = false) var env =
  match var with
  | VarName n ->
    let* v = Env.get_value n env in
    Ok (typecheck_value v env)
  | VarTableField (pexp, ((l, _) as exp)) -> (
    let* t_pexp = typecheck_prefixexp pexp env in
    let* t_exp = typecheck_expr exp env in
    match t_pexp with
    | Ttable ->
      if t_exp = Tnil then error (Some l) "table index is nil" else Ok Ttable
    | Tref Ttable -> Ok (Tref Ttable)
    | TfunctionReturn (Ttable :: _) as tfr -> Ok tfr
    | TfunctionReturn _ -> error (Some l) "attempt to index a non table value"
    | t ->
      if strict then error (Some l) "attempt to index a non table value"
      else Ok t )

and typecheck_prefixexp pexp env =
  match pexp with
  | PEvar v -> typecheck_var v env
  | PEfunctioncall (FCpreargs (pexp, _)) -> typecheck_prefixexp pexp env
  | PEfunctioncall (FCprename (pexp, _, _)) -> typecheck_prefixexp pexp env
  | PEexp e -> typecheck_expr e env

and typecheck_expr expr env =
  match snd expr with
  | Evalue v -> Ok (typecheck_value v env)
  | Eunop (Unot, _) -> Ok Tboolean
  | Eunop (Uminus, e) -> typecheck_arith_unop e env
  | Eunop (Usharp, e) -> typecheck_sharp_unop e env
  | Eunop (Ulnot, e) -> typecheck_bitwise_unop e env
  | Ebinop (_, Band, _) | Ebinop (_, Bor, _) ->
    Ok Tboolean (* Nb. all types are possible! *)
  | Ebinop (e1, ((Badd | Bsub | Bmul | Bdiv | Bfldiv | Bmod | Bexp) as op), e2)
    ->
    typecheck_arith_binop op e1 e2 env
  | Ebinop (e1, (Bland | Blor | Blxor | Blsl | Blsr), e2) ->
    typecheck_bitwise_binop e1 e2 env
  | Ebinop (e1, ((Blt | Ble | Bgt | Bge | Beq | Bneq) as op), e2) ->
    typecheck_rel_binop op e1 e2 env
  | Ebinop (e1, Bddot, e2) -> typecheck_str_binop e1 e2 env
  | Evariadic ->
    Ok (Tvariadic [])
    (* wip: Tvariadic [] not fully correct > improve Evariadic & parlist types ? *)
  | Efunctiondef _ -> Ok Tfunction
  | Eprefix pexp -> typecheck_prefixexp pexp env
  | Etableconstructor _ -> Ok Ttable

and typecheck_for_ctrl_expr ((loc, _e) as expr) env =
  let* t = typecheck_expr expr env in
  match t with
  | Tnumber _ -> Ok t
  | _ -> error (Some loc) "bad 'for' initial, limit, step (number expected)"

and typecheck_iterator_ctrl_el el env =
  match el with
  | e :: _ ->
    let l, _ = e in
    let* t = typecheck_expr e env in
    begin match t with
    | Tfunction | TfunctionStdLib -> Ok [ t ]
    | _ ->
      error (Some l)
        "attempt to call a non function value (for iterator 'for iterator')"
    end
  | [] -> (* Syntax error *) assert false
