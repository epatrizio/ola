(* Typer *)

open Ast

exception Typing_error of location * string

let error loc message = raise (Typing_error (loc, message))

let ( let* ) = Result.bind

let list_iter fct list =
  try
    List.iter
      (fun (_, elt) ->
        match fct elt with
        | Error (loc, message) -> error loc message
        | Ok () -> () )
      list;
    Ok ()
  with Typing_error (loc, message) -> Error (loc, message)

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
    let loc, _expr = expr in
    let typ = typecheck_expr expr in
    match typ with
    | Tnumber Tinteger -> Tnumber Tinteger
    | Tnumber Tfloat -> Tnumber Tfloat
    | Tnil -> error loc "attempt to perform arithmetic on a nil value"
    | Tboolean -> error loc "attempt to perform arithmetic on a boolean value"
    | Tstring -> error loc "attempt to perform arithmetic on a string value"
    | _ -> assert false (* call error *)
  in
  let typecheck_bitwise_unop expr =
    let loc, _expr = expr in
    let typ = typecheck_expr expr in
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
  let typecheck_arith_binop binop expr1 expr2 =
    let loc1, _expr1 = expr1 in
    let loc2, _expr2 = expr2 in
    let typ1 = typecheck_expr expr1 in
    let typ2 = typecheck_expr expr2 in
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
  let typecheck_bitwise_binop expr1 expr2 =
    let loc1, _expr1 = expr1 in
    let loc2, _expr2 = expr2 in
    let typ1 = typecheck_expr expr1 in
    let typ2 = typecheck_expr expr2 in
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
  let typecheck_str_binop expr1 expr2 =
    let loc1, _expr1 = expr1 in
    let loc2, _expr2 = expr2 in
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
    | Tnil, _ -> error loc1 "attempt to concatenate a nil value"
    | _, Tnil -> error loc2 "attempt to concatenate a nil value"
    | Tboolean, _ -> error loc1 "attempt to concatenate a boolean value"
    | _, Tboolean -> error loc2 "attempt to concatenate a boolean value"
    | _ -> assert false (* call error *)
  in
  match expr with
  | _loc, Evalue v -> typecheck_value v
  | _loc, Evar _v -> Tnil (* TODO *)
  | _loc, Eunop (Unot, _) -> Tboolean
  | _loc, Eunop (Uminus, e) -> typecheck_arith_unop e
  | _loc, Eunop (Usharp, e) ->
    let l, _e = e in
    let typ = typecheck_expr e in
    begin
      match typ with
      | Tstring -> Tnumber Tinteger
      | Tnil | Tboolean | Tnumber _ ->
        error l "attempt to perform #operator on a not supported type"
      | _ -> error l "#operator on this type: to be implemented ..."
    end
  | _loc, Eunop (Ulnot, e) -> typecheck_bitwise_unop e
  | _loc, Ebinop (Band, _, _) | _loc, Ebinop (Bor, _, _) -> Tboolean (* TODO *)
  | _loc, Ebinop (Badd, e1, e2) -> typecheck_arith_binop Badd e1 e2
  | _loc, Ebinop (Bsub, e1, e2) -> typecheck_arith_binop Bsub e1 e2
  | _loc, Ebinop (Bmul, e1, e2) -> typecheck_arith_binop Bmul e1 e2
  | _loc, Ebinop (Bdiv, e1, e2) -> typecheck_arith_binop Bdiv e1 e2
  | _loc, Ebinop (Bfldiv, e1, e2) -> typecheck_arith_binop Bfldiv e1 e2
  | _loc, Ebinop (Bmod, e1, e2) -> typecheck_arith_binop Bmod e1 e2
  | _loc, Ebinop (Bexp, e1, e2) -> typecheck_arith_binop Bexp e1 e2
  | _loc, Ebinop (Bland, e1, e2)
  | _loc, Ebinop (Blor, e1, e2)
  | _loc, Ebinop (Blxor, e1, e2)
  | _loc, Ebinop (Blsl, e1, e2)
  | _loc, Ebinop (Blsr, e1, e2) ->
    typecheck_bitwise_binop e1 e2
  | _loc, Ebinop (Blt, _, _)
  | _loc, Ebinop (Ble, _, _)
  | _loc, Ebinop (Bgt, _, _)
  | _loc, Ebinop (Bge, _, _)
  | _loc, Ebinop (Beq, _, _)
  | _loc, Ebinop (Bneq, _, _) ->
    Tboolean (* TODO *)
  | _loc, Ebinop (Bddot, e1, e2) -> typecheck_str_binop e1 e2

let rec typecheck_stmt stmt =
  try
    match stmt with
    | Sempty -> Ok ()
    | Sassign (_il, _el) -> Ok () (* todo: to be implemented *)
    | SassignLocal (_nal, _elo) -> Ok () (* todo: to be implemented *)
    | Sbreak -> Ok ()
    | Slabel _ -> Ok ()
    | Sgoto _ -> Ok ()
    | Sblock b -> typecheck_block b
    | Swhile (_e, b) ->
      (* Doc: The condition expression _e of a control structure can return any value *)
      typecheck_block b
    | Srepeat (b, _e) -> typecheck_block b (* Memo: Same Swhile *)
    | Sif (_e, b, ebl, ob) ->
      (* Memo: Same Swhile *)
      let* _ = typecheck_block b in
      let* _ = list_iter typecheck_block ebl in
      begin
        match ob with None -> Ok () | Some b -> typecheck_block b
      end
    | Sfor (_n, e1, e2, oe, b) ->
      let typecheck_init expr =
        begin
          match typecheck_expr expr with
          | Tnumber Tinteger | Tnumber Tfloat | Tstring ->
            (* string will be cast in float value during interpretation *)
            Ok ()
          | _ ->
            let loc, _e = expr in
            error loc "bad 'for' initial, limit, step (number expected)"
        end
      in
      let* _ = typecheck_init e1 in
      let* _ = typecheck_init e2 in
      begin
        match oe with
        | Some e ->
          let* _ = typecheck_init e in
          typecheck_block b
        | None -> typecheck_block b
      end
    | Siterator (_nl, _el, _b) -> Ok () (* todo: to be implemented *)
    | Sprint e ->
      let _ = typecheck_expr e in
      Ok ()
  with Typing_error (loc, message) -> Error (loc, message)

and typecheck_block b =
  match b with
  | [] -> Ok ()
  | s :: tl -> (
    match typecheck_stmt s with Ok () -> typecheck_block tl | e -> e )

let typecheck chunk = typecheck_block chunk
