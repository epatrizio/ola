(* Typer *)

(* Note :
   Now typecheck is performed during interpretation.
   So, statement typecheck is no longer used.
*)

open Ast

let error loc message = Error (loc, message)

let ( let* ) = Result.bind

let rec typecheck_value value =
  match value with
  | Vnil () -> Tnil
  | Vboolean _ -> Tboolean
  | Vnumber (Ninteger _) -> Tnumber Tinteger
  | Vnumber (Nfloat _) -> Tnumber Tfloat
  | Vstring _ -> Tstring
  | Vfunction _ -> Tfunction
  | VfunctionReturn vl -> TfunctionReturn (List.map typecheck_value vl)
  | Vtable _ -> Ttable

let rec typecheck_arith_unop ((loc, _e) as expr) env =
  let* t = typecheck_expr expr env in
  match t with
  | Tnumber Tinteger -> Ok (Tnumber Tinteger)
  | Tnumber Tfloat -> Ok (Tnumber Tfloat)
  | Tstring ->
    Ok (Tnumber Tinteger) (* or Tfloat - cast & check during interpretation *)
  | Tnil -> error loc "attempt to perform arithmetic on a nil value"
  | Tboolean -> error loc "attempt to perform arithmetic on a boolean value"
  | _ -> assert false (* call error *)

and typecheck_bitwise_unop ((loc, _e) as expr) env =
  let* t = typecheck_expr expr env in
  match t with
  | Tnumber Tinteger -> Ok (Tnumber Tinteger)
  | Tnumber Tfloat ->
    Ok (Tnumber Tfloat)
    (* must be an integer (ex: 42.0) : check during interpretation *)
  | Tnil -> error loc "attempt to perform bitwise operation on a nil value"
  | Tboolean ->
    error loc "attempt to perform bitwise operation on a boolean value"
  | Tstring ->
    error loc "attempt to perform bitwise operation on a string value"
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
  | Tnil, _ -> error loc1 "attempt to perform arithmetic on a nil value"
  | _, Tnil -> error loc2 "attempt to perform arithmetic on a nil value"
  | Tboolean, _ -> error loc1 "attempt to perform arithmetic on a boolean value"
  | _, Tboolean -> error loc2 "attempt to perform arithmetic on a boolean value"
  | Tstring, _ -> error loc1 "attempt to perform arithmetic on a string value"
  | _, Tstring -> error loc2 "attempt to perform arithmetic on a string value"
  | _ -> assert false (* call error *)

and typecheck_bitwise_binop ((loc1, _e1) as expr1) ((loc2, _e2) as expr2) env =
  let* typ1 = typecheck_expr expr1 env in
  let* typ2 = typecheck_expr expr2 env in
  match (typ1, typ2) with
  | Tnumber _, Tnumber _ -> Ok (Tnumber Tinteger)
  | Tnil, _ -> error loc1 "attempt to perform bitwise operation on a nil value"
  | _, Tnil -> error loc2 "attempt to perform bitwise operation on a nil value"
  | Tboolean, _ ->
    error loc1 "attempt to perform bitwise operation on a boolean value"
  | _, Tboolean ->
    error loc2 "attempt to perform bitwise operation on a boolean value"
  | Tstring, _ ->
    error loc1 "attempt to perform bitwise operation on a string value"
  | _, Tstring ->
    error loc2 "attempt to perform bitwise operation on a string value"
  | _ -> assert false (* call error *)

and typecheck_str_binop ((loc1, _e1) as expr1) ((loc2, _e2) as expr2) env =
  let* typ1 = typecheck_expr expr1 env in
  let* typ2 = typecheck_expr expr2 env in
  match (typ1, typ2) with
  | (Tstring | Tnumber _), (Tstring | Tnumber _) -> Ok Tstring
  | Tnil, _ -> error loc1 "attempt to concatenate a nil value"
  | _, Tnil -> error loc2 "attempt to concatenate a nil value"
  | Tboolean, _ -> error loc1 "attempt to concatenate a boolean value"
  | _, Tboolean -> error loc2 "attempt to concatenate a boolean value"
  | _ -> assert false (* call error *)

and typecheck_var var env =
  match var with
  | VarName n ->
    let v = Env.get_value n env in
    Ok (typecheck_value v)
  | VarTableField (_pexp, _exp) -> Ok Tnil (* TODO *)

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
  | Eunop (Usharp, ((l, _e) as e)) ->
    let* typ = typecheck_expr e env in
    begin
      match typ with
      | Tstring -> Ok (Tnumber Tinteger)
      | Tnil | Tboolean | Tnumber _ ->
        error l "attempt to perform #operator on a not supported type"
      | _ -> error l "#operator on this type: to be implemented ..."
    end
  | Eunop (Ulnot, e) -> typecheck_bitwise_unop e env
  | Ebinop (Band, _, _) | Ebinop (Bor, _, _) ->
    Ok Tnil (* Nb. all types are possible! *)
  | Ebinop (((Badd | Bsub | Bmul | Bdiv | Bfldiv | Bmod | Bexp) as op), e1, e2)
    ->
    typecheck_arith_binop op e1 e2 env
  | Ebinop ((Bland | Blor | Blxor | Blsl | Blsr), e1, e2) ->
    typecheck_bitwise_binop e1 e2 env
  | Ebinop ((Blt | Ble | Bgt | Bge | Beq | Bneq), _, _) ->
    Ok Tboolean (* TODO *)
  | Ebinop (Bddot, e1, e2) -> typecheck_str_binop e1 e2 env
  | Evariadic -> Ok Tnil (* TODO: OK ? *)
  | Efunctiondef _ -> Ok Tfunction (* TODO: OK ? *)
  | Eprefix pexp -> typecheck_prefixexp pexp env
  | Etableconstructor _ -> Ok Ttable (* TODO: OK ? *)

and typecheck_lexpr lexpr env =
  List.fold_left
    (fun acc e ->
      let* () = acc in
      let* _ = typecheck_expr e env in
      Ok () )
    (Ok ()) lexpr

and typecheck_functioncall _fc _env = Ok Tnil
(* todo ?? *)
(* and typecheck_functioncall (FCpreargs (((loc, _e) as e), el)) env =
   let* typ = typecheck_expr e env in
   match typ with
   | Tfunction ->
     let* () = typecheck_lexpr el env in
     Ok Tnil
   | _ -> error loc "attempt to call a not function value" *)

and typecheck_stmt stmt env =
  match stmt with
  | Sempty -> Ok ()
  | Sassign (_il, _el) -> Ok () (* todo: to be implemented *)
  | SassignLocal (_nal, _elo) -> Ok () (* todo: to be implemented *)
  | Sbreak -> Ok ()
  | Sreturn (_el, _so) -> Ok ()
  | Slabel _ -> Ok ()
  | Sgoto _ -> Ok ()
  | Sblock b -> typecheck_block b env
  | Swhile (_e, b) ->
    (* Doc: The condition expression _e of a control structure can return any value *)
    typecheck_block b env
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
    begin
      match ob with None -> Ok () | Some b -> typecheck_block b env
    end
  | Sfor (_n, e1, e2, oe, b) ->
    let typecheck_init expr env =
      let* t = typecheck_expr expr env in
      match t with
      | Tnumber Tinteger | Tnumber Tfloat | Tstring ->
        (* string will be cast in float value during interpretation *)
        Ok ()
      | _ ->
        let loc, _e = expr in
        error loc "bad 'for' initial, limit, step (number expected)"
    in
    let* () = typecheck_init e1 env in
    let* () = typecheck_init e2 env in
    begin
      match oe with
      | Some e ->
        let* () = typecheck_init e env in
        typecheck_block b env
      | None -> typecheck_block b env
    end
  | Siterator (_nl, _el, _b) ->
    Ok ()
    (* todo: to be implemented *)
    (* | Sfunction (_n, (_pl, b)) ->
       typecheck_block b env *)
    (* todo: to be continued *)
    (* | SfunctionLocal (_n, (_pl, b)) ->
       typecheck_block b env *)
    (* todo: to be continued *)
  | SfunctionCall fc ->
    let* (_ : Ast.typ) = typecheck_functioncall fc env in
    Ok ()
  | Sprint e ->
    let* (_ : Ast.typ) = typecheck_expr e env in
    Ok ()

and typecheck_block b env =
  List.fold_left
    (fun acc s ->
      let* () = acc in
      typecheck_stmt s env )
    (Ok ()) b
