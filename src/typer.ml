(* Typer *)

(* Note :
   Now typecheck is performed during interpretation on each expressions.
   So, statement typecheck is no longer used, except Sfor for init values.
*)

open Ast
open Syntax

exception Typing_error of Ast.location option * string

let error loc_opt message = raise (Typing_error (loc_opt, message))

let rec typecheck_value value =
  match value with
  | Vnil () -> Tnil
  | Vboolean _ -> Tboolean
  | Vnumber (Ninteger _) -> Tnumber Tinteger
  | Vnumber (Nfloat _) -> Tnumber Tfloat
  | Vstring _ -> Tstring
  | Vvariadic vl -> Tvariadic (List.map typecheck_value vl)
  | Vfunction _ -> Tfunction
  | VfunctionReturn vl -> TfunctionReturn (List.map typecheck_value vl)
  | VfunctionStdLib _ -> TfunctionStdLib
  | Vtable _ -> Ttable

let rec typecheck_arith_unop ((loc, _e) as expr) env =
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

and typecheck_var var env =
  match var with
  | VarName n -> begin
    match Env.get_value n env with
    | Ok v -> Ok (typecheck_value v)
    | Error msg -> error None ("Env error: " ^ msg)
  end
  | VarTableField (pexp, (l, _e)) -> (
    let* t = typecheck_prefixexp pexp env in
    match t with
    | Ttable -> Ok Ttable (* col table type check during interpretation *)
    | TfunctionReturn _ ->
      (* WIP *)
      Ok (TfunctionReturn [])
      (* function call return type check during interpretation *)
    | _ -> error (Some l) "attempt to access a non table value" )

and typecheck_prefixexp pexp env =
  match pexp with
  | PEvar v -> typecheck_var v env
  | PEfunctioncall fc -> typecheck_functioncall fc env
  | PEexp e -> typecheck_expr e env

and typecheck_expr expr env =
  match snd expr with
  | Evalue v -> Ok (typecheck_value v)
  | Eunop (Unot, _) -> Ok Tboolean
  | Eunop (Uminus, e) -> typecheck_arith_unop e env
  | Eunop (Usharp, e) -> typecheck_sharp_unop e env
  | Eunop (Ulnot, e) -> typecheck_bitwise_unop e env
  | Ebinop (_, Band, _) | Ebinop (_, Bor, _) ->
    Ok Tnil (* Nb. all types are possible! *)
  | Ebinop (e1, ((Badd | Bsub | Bmul | Bdiv | Bfldiv | Bmod | Bexp) as op), e2)
    ->
    typecheck_arith_binop op e1 e2 env
  | Ebinop (e1, (Bland | Blor | Blxor | Blsl | Blsr), e2) ->
    typecheck_bitwise_binop e1 e2 env
  | Ebinop (e1, ((Blt | Ble | Bgt | Bge | Beq | Bneq) as op), e2) ->
    typecheck_rel_binop op e1 e2 env
  | Ebinop (e1, Bddot, e2) -> typecheck_str_binop e1 e2 env
  | Evariadic ->
    Ok (Tvariadic []) (* type check explicit during interpretation *)
  | Efunctiondef _ -> Ok Tfunction
  | Eprefix pexp -> typecheck_prefixexp pexp env
  | Etableconstructor _ -> Ok Ttable

and typecheck_functioncall fc env =
  let typ_check t =
    match t with
    | Ok Tfunction -> Ok (TfunctionReturn []) (* WIP *)
    | Ok TfunctionStdLib -> Ok TfunctionStdLib
    | Ok Ttable -> Ok Ttable (* col table type check during interpretation *)
    | Error (l, mes) -> error l mes
    | _ -> error None "attempt to call a non function value"
  in
  match fc with
  | FCpreargs (PEvar v, _) ->
    let t = typecheck_var v env in
    typ_check t
  | FCpreargs (PEfunctioncall fc, _) -> typecheck_functioncall fc env
  | FCpreargs (PEexp e, _) ->
    let t = typecheck_expr e env in
    typ_check t
  | FCprename (pe, _, _) ->
    let t = typecheck_prefixexp pe env in
    typ_check t

and typecheck_stmt stmt env =
  match stmt with
  | Sempty -> Ok ()
  | Sassign _ -> Ok ()
  | SassignLocal _ -> Ok ()
  | Sbreak -> Ok ()
  | Sreturn _ -> Ok () (* WIP: return type list ? *)
  | Slabel _ -> Ok ()
  | Sgoto _ -> Ok ()
  | Sblock b -> typecheck_block b env
  | Swhile (_e, b) ->
    typecheck_block b env
    (* Doc: The condition expression _e of a control structure can return any value *)
  | Srepeat (b, _e) -> typecheck_block b env (* Memo: Same Swhile *)
  | Sif (_e, b, ebl, ob) ->
    (* Memo: Same Swhile *)
    let* () = typecheck_block b env in
    let* () =
      List.fold_left
        (fun acc (_e, b) ->
          let* () = acc in
          typecheck_block b env )
        (Ok ()) ebl
    in
    begin match ob with None -> Ok () | Some b -> typecheck_block b env
    end
  | Sfor (_n, e1, e2, oe, _b) ->
    let typecheck_init ((loc, _e) as expr) env =
      let* t = typecheck_expr expr env in
      match t with
      | Tnumber Tinteger | Tnumber Tfloat | Tstring ->
        (* string will be cast in float value during interpretation *)
        Ok ()
      | _ -> error (Some loc) "bad 'for' initial, limit, step (number expected)"
    in
    let* () = typecheck_init e1 env in
    let* () = typecheck_init e2 env in
    begin match oe with Some e -> typecheck_init e env | None -> Ok ()
    end
  | Siterator (_nl, el, _b) ->
    let typecheck_e ((loc, _e) as expr) env =
      let* t = typecheck_expr expr env in
      match t with
      | Tfunction | TfunctionStdLib -> Ok ()
      | TfunctionReturn tl -> begin
        match tl with
        | [] -> Ok () (* WIP: OK ? *)
        | [ Tfunction ] | [ TfunctionStdLib ] -> Ok ()
        | Tfunction :: _el | TfunctionStdLib :: _el -> Ok ()
        | _ ->
          error (Some loc) "bad 'for iterator' (iterator function expected)"
      end
      | _ -> error (Some loc) "bad 'for iterator' (iterator function expected)"
    in
    begin match el with
    | [ e ] -> typecheck_e e env
    | e :: _el -> typecheck_e e env
    | [] -> assert false (* syntax error *)
    end
  | SfunctionCall fc ->
    let* (_ : Ast.typ) = typecheck_functioncall fc env in
    Ok ()

and typecheck_block b env =
  List.fold_left
    (fun acc s ->
      let* () = acc in
      typecheck_stmt s env )
    (Ok ()) b
