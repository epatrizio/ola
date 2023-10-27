(* Typer *)

(* Note :
   Now typecheck is performed during interpretation.
   So, statement typecheck is no longer used.
*)

open Ast

exception Typing_error of location * string

let error loc message = raise (Typing_error (loc, message))

let ( let* ) = Result.bind

let list_iter fct list env =
  try
    List.iter
      (fun (_, elt) ->
        match fct elt env with
        | Error (loc, message) -> error loc message
        | Ok () -> () )
      list;
    Ok ()
  with Typing_error (loc, message) -> Error (loc, message)

let rec typecheck_expr expr env =
  let typecheck_value value =
    match value with
    | Vnil () -> Tnil
    | Vboolean _ -> Tboolean
    | Vnumber (Ninteger _) -> Tnumber Tinteger
    | Vnumber (Nfloat _) -> Tnumber Tfloat
    | Vstring _ -> Tstring
  in
  let typecheck_arith_unop expr env =
    let loc, _expr = expr in
    let typ = typecheck_expr expr env in
    match typ with
    | Tnumber Tinteger -> Tnumber Tinteger
    | Tnumber Tfloat -> Tnumber Tfloat
    | Tnil -> error loc "attempt to perform arithmetic on a nil value"
    | Tboolean -> error loc "attempt to perform arithmetic on a boolean value"
    | Tstring -> error loc "attempt to perform arithmetic on a string value"
    | _ -> assert false (* call error *)
  in
  let typecheck_bitwise_unop expr env =
    let loc, _expr = expr in
    let typ = typecheck_expr expr env in
    match typ with
    | Tnumber Tinteger -> Tnumber Tinteger
    | Tnumber Tfloat ->
      Tnumber
        Tfloat (* must be an integer (ex: 42.0) : check during interpretation *)
    | Tnil -> error loc "attempt to perform bitwise operation on a nil value"
    | Tboolean ->
      error loc "attempt to perform bitwise operation on a boolean value"
    | Tstring ->
      error loc "attempt to perform bitwise operation on a string value"
    | _ -> assert false (* call error *)
  in
  let typecheck_arith_binop binop expr1 expr2 env =
    let loc1, _expr1 = expr1 in
    let loc2, _expr2 = expr2 in
    let typ1 = typecheck_expr expr1 env in
    let typ2 = typecheck_expr expr2 env in
    match (typ1, typ2) with
    | Tnumber Tinteger, Tnumber Tinteger ->
      if binop = Bdiv || binop = Bexp then Tnumber Tfloat else Tnumber Tinteger
    | Tnumber Tfloat, Tnumber Tfloat
    | Tnumber Tinteger, Tnumber Tfloat
    | Tnumber Tfloat, Tnumber Tinteger ->
      Tnumber Tfloat
    | Tnil, _ -> error loc1 "attempt to perform arithmetic on a nil value"
    | _, Tnil -> error loc2 "attempt to perform arithmetic on a nil value"
    | Tboolean, _ ->
      error loc1 "attempt to perform arithmetic on a boolean value"
    | _, Tboolean ->
      error loc2 "attempt to perform arithmetic on a boolean value"
    | Tstring, _ -> error loc1 "attempt to perform arithmetic on a string value"
    | _, Tstring -> error loc2 "attempt to perform arithmetic on a string value"
    | _ -> assert false (* call error *)
  in
  let typecheck_bitwise_binop expr1 expr2 env =
    let loc1, _expr1 = expr1 in
    let loc2, _expr2 = expr2 in
    let typ1 = typecheck_expr expr1 env in
    let typ2 = typecheck_expr expr2 env in
    match (typ1, typ2) with
    | Tnumber Tinteger, Tnumber Tinteger
    | Tnumber Tfloat, Tnumber Tfloat
    | Tnumber Tinteger, Tnumber Tfloat
    | Tnumber Tfloat, Tnumber Tinteger ->
      Tnumber Tinteger
    | Tnil, _ ->
      error loc1 "attempt to perform bitwise operation on a nil value"
    | _, Tnil ->
      error loc2 "attempt to perform bitwise operation on a nil value"
    | Tboolean, _ ->
      error loc1 "attempt to perform bitwise operation on a boolean value"
    | _, Tboolean ->
      error loc2 "attempt to perform bitwise operation on a boolean value"
    | Tstring, _ ->
      error loc1 "attempt to perform bitwise operation on a string value"
    | _, Tstring ->
      error loc2 "attempt to perform bitwise operation on a string value"
    | _ -> assert false (* call error *)
  in
  let typecheck_str_binop expr1 expr2 env =
    let loc1, _expr1 = expr1 in
    let loc2, _expr2 = expr2 in
    let typ1 = typecheck_expr expr1 env in
    let typ2 = typecheck_expr expr2 env in
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
    | Tnil, _ -> error loc1 "attempt to concatenate a nil value"
    | _, Tnil -> error loc2 "attempt to concatenate a nil value"
    | Tboolean, _ -> error loc1 "attempt to concatenate a boolean value"
    | _, Tboolean -> error loc2 "attempt to concatenate a boolean value"
    | _ -> assert false (* call error *)
  in
  match expr with
  | _loc, Evalue v -> typecheck_value v
  | _loc, Eunop (Unot, _) -> Tboolean
  | _loc, Eunop (Uminus, e) -> typecheck_arith_unop e env
  | _loc, Eunop (Usharp, e) ->
    let l, _e = e in
    let typ = typecheck_expr e env in
    begin
      match typ with
      | Tstring -> Tnumber Tinteger
      | Tnil | Tboolean | Tnumber _ ->
        error l "attempt to perform #operator on a not supported type"
      | _ -> error l "#operator on this type: to be implemented ..."
    end
  | _loc, Eunop (Ulnot, e) -> typecheck_bitwise_unop e env
  | _loc, Ebinop (Band, _, _) | _loc, Ebinop (Bor, _, _) -> Tboolean (* TODO *)
  | _loc, Ebinop (Badd, e1, e2) -> typecheck_arith_binop Badd e1 e2 env
  | _loc, Ebinop (Bsub, e1, e2) -> typecheck_arith_binop Bsub e1 e2 env
  | _loc, Ebinop (Bmul, e1, e2) -> typecheck_arith_binop Bmul e1 e2 env
  | _loc, Ebinop (Bdiv, e1, e2) -> typecheck_arith_binop Bdiv e1 e2 env
  | _loc, Ebinop (Bfldiv, e1, e2) -> typecheck_arith_binop Bfldiv e1 e2 env
  | _loc, Ebinop (Bmod, e1, e2) -> typecheck_arith_binop Bmod e1 e2 env
  | _loc, Ebinop (Bexp, e1, e2) -> typecheck_arith_binop Bexp e1 e2 env
  | _loc, Ebinop (Bland, e1, e2)
  | _loc, Ebinop (Blor, e1, e2)
  | _loc, Ebinop (Blxor, e1, e2)
  | _loc, Ebinop (Blsl, e1, e2)
  | _loc, Ebinop (Blsr, e1, e2) ->
    typecheck_bitwise_binop e1 e2 env
  | _loc, Ebinop (Blt, _, _)
  | _loc, Ebinop (Ble, _, _)
  | _loc, Ebinop (Bgt, _, _)
  | _loc, Ebinop (Bge, _, _)
  | _loc, Ebinop (Beq, _, _)
  | _loc, Ebinop (Bneq, _, _) ->
    Tboolean (* TODO *)
  | _loc, Ebinop (Bddot, e1, e2) -> typecheck_str_binop e1 e2 env
  | _loc, Evariadic -> Tnil (* TODO: OK ? *)
  | _loc, Efunctiondef _ -> Tfunction (* TODO: OK ? *)
  | _loc, Eprefix (PEvar (Name n)) ->
    let v = Env.get_value n env in
    typecheck_value v
  | _loc, Eprefix (PEexp e) -> typecheck_expr e env
  | _loc, Eprefix (PEfunctioncall fc) -> typecheck_functioncall fc env

and typecheck_lexpr lexpr env =
  try
    List.iter
      (fun e ->
        let _ = typecheck_expr e env in
        () )
      lexpr;
    Ok ()
  with Typing_error (loc, message) -> Error (loc, message)

and typecheck_functioncall fc env =
  let (FCpreargs (e, Aexplist el)) = fc in
  let loc, _e = e in
  let typ = typecheck_expr e env in
  begin
    match typ with
    | Tfunction -> begin
      match typecheck_lexpr el env with
      | Ok () ->
        Tnil (* TODO: Here, call is valid > do typecheck functioncall *)
      | Error (loc, message) -> error loc message
    end
    | _ -> error loc "attempt to call a not function value"
  end

and typecheck_stmt stmt env =
  try
    match stmt with
    | Sempty -> Ok ()
    | Sassign (_il, _el) -> Ok () (* todo: to be implemented *)
    | SassignLocal (_nal, _elo) -> Ok () (* todo: to be implemented *)
    | Sbreak -> Ok ()
    | Sreturn (_elo, _so) -> Ok ()
    | Slabel _ -> Ok ()
    | Sgoto _ -> Ok ()
    | Sblock b -> typecheck_block b env
    | Swhile (_e, b) ->
      (* Doc: The condition expression _e of a control structure can return any value *)
      typecheck_block b env
    | Srepeat (b, _e) -> typecheck_block b env (* Memo: Same Swhile *)
    | Sif (_e, b, ebl, ob) ->
      (* Memo: Same Swhile *)
      let* _ = typecheck_block b env in
      let* _ = list_iter typecheck_block ebl env in
      begin
        match ob with None -> Ok () | Some b -> typecheck_block b env
      end
    | Sfor (_n, e1, e2, oe, b) ->
      let typecheck_init expr env =
        begin
          match typecheck_expr expr env with
          | Tnumber Tinteger | Tnumber Tfloat | Tstring ->
            (* string will be cast in float value during interpretation *)
            Ok ()
          | _ ->
            let loc, _e = expr in
            error loc "bad 'for' initial, limit, step (number expected)"
        end
      in
      let* _ = typecheck_init e1 env in
      let* _ = typecheck_init e2 env in
      begin
        match oe with
        | Some e ->
          let* _ = typecheck_init e env in
          typecheck_block b env
        | None -> typecheck_block b env
      end
    | Siterator (_nl, _el, _b) -> Ok () (* todo: to be implemented *)
    | Sfunction (_n, (_pl, b)) ->
      typecheck_block b env (* todo: to be continued *)
    | SfunctionLocal (_n, (_pl, b)) ->
      typecheck_block b env (* todo: to be continued *)
    | SfunctionCall fc ->
      let _ = typecheck_functioncall fc env in
      Ok ()
    | Sprint e ->
      let _ = typecheck_expr e env in
      Ok ()
  with Typing_error (loc, message) -> Error (loc, message)

and typecheck_block b env =
  match b with
  | [] -> Ok ()
  | s :: tl -> (
    match typecheck_stmt s env with Ok () -> typecheck_block tl env | e -> e )

let typecheck chunk env = typecheck_block chunk env
