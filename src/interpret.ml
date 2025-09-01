open Ast
open Syntax
open Typer

let () = Random.self_init ()

type block_pointer =
  | Begin
  | Last
  | Label of string

exception Goto_catch of block_pointer * value Env.t

exception Break_catch of value Env.t

exception Return_catch of expr list * value Env.t

exception Interpretation_error of location option * string

let error loc_opt message = raise (Interpretation_error (loc_opt, message))

let env_result_check res =
  match res with
  | Ok v -> Ok v
  | Error message -> error None (Format.sprintf "Env error: %s" message)

let rec block_from_pointer pt stmt_list =
  match (pt, stmt_list) with
  | Begin, _ -> stmt_list
  | Last, ([] | [ _ ]) -> stmt_list
  | Last, _stmt :: tl -> block_from_pointer Last tl
  | Label l, [] -> error None ("no visible label for <goto> " ^ l)
  | Label l, Slabel n :: tl when l = n -> tl
  | Label l, _stmt :: tl -> block_from_pointer (Label l) tl

let get_int f loc =
  if Float.is_integer f then int_of_float f
  else
    error (Some loc)
      ("number has no integer representation: " ^ string_of_float f)

let number_of_string loc str =
  match int_of_string_opt str with
  | Some i -> Vnumber (Ninteger i)
  | None -> (
    match float_of_string_opt str with
    | Some f -> Vnumber (Nfloat f)
    | None ->
      error loc (Format.sprintf "attempt to perform on a string (%s) value" str)
    )

let get_int_value_opt v =
  match v with Vnumber (Ninteger i) when i > 0 -> Some i | _ -> None

let rec interpret_bbinop_expr binop expr1 expr2 env =
  let* v1, env = interpret_expr expr1 env in
  match binop with
  | Band -> begin
    match v1 with
    | Vboolean false | Vnil () -> Ok (v1, env)
    | _ -> interpret_expr expr2 env (* short-circuit evaluation *)
  end
  | Bor -> begin
    match v1 with
    | v when v <> Vnil () && v <> Vboolean false -> Ok (v1, env)
    | _ -> interpret_expr expr2 env (* short-circuit evaluation *)
  end
  | _ -> assert false (* call error *)

and interpret_arith_binop_expr binop ((loc1, _) as expr1) ((loc2, _) as expr2)
  env =
  let* v1, env = interpret_expr expr1 env in
  let* v2, env = interpret_expr expr2 env in
  let* _ =
    typecheck_expr
      (loc1, Ebinop ((loc1, Evalue v1), binop, (loc2, Evalue v2)))
      env
  in
  let v1 =
    match v1 with Vstring s -> number_of_string (Some loc1) s | v -> v
  in
  let v2 =
    match v2 with Vstring s -> number_of_string (Some loc2) s | v -> v
  in
  match (v1, v2) with
  | Vnumber (Ninteger i1), Vnumber (Ninteger i2) -> begin
    match binop with
    | Badd -> Ok (Vnumber (Ninteger (i1 + i2)), env)
    | Bsub -> Ok (Vnumber (Ninteger (i1 - i2)), env)
    | Bmul -> Ok (Vnumber (Ninteger (i1 * i2)), env)
    | Bdiv -> Ok (Vnumber (Nfloat (float_of_int i1 /. float_of_int i2)), env)
    | Bfldiv -> Ok (Vnumber (Ninteger (i1 / i2)), env (* todo div by 0 error *))
    | Bmod -> Ok (Vnumber (Ninteger (i1 mod i2)), env)
    | Bexp ->
      Ok (Vnumber (Nfloat (Float.pow (float_of_int i1) (float_of_int i2))), env)
    | _ -> assert false (* call error *)
  end
  | Vnumber (Nfloat f), Vnumber (Ninteger i) -> begin
    match binop with
    | Badd -> Ok (Vnumber (Nfloat (f +. float_of_int i)), env)
    | Bsub -> Ok (Vnumber (Nfloat (f -. float_of_int i)), env)
    | Bmul -> Ok (Vnumber (Nfloat (f *. float_of_int i)), env)
    | Bdiv -> Ok (Vnumber (Nfloat (f /. float_of_int i)), env)
    | Bfldiv ->
      let _, q = Float.modf (f /. float_of_int i) in
      Ok (Vnumber (Nfloat q), env)
    | Bmod ->
      let fi = float_of_int i in
      let _, q = Float.modf (f /. fi) in
      Ok (Vnumber (Nfloat (f -. (fi *. q))), env)
    | Bexp -> Ok (Vnumber (Nfloat (Float.pow f (float_of_int i))), env)
    | _ -> assert false (* call error *)
  end
  | Vnumber (Ninteger i), Vnumber (Nfloat f) -> begin
    match binop with
    | Badd -> Ok (Vnumber (Nfloat (float_of_int i +. f)), env)
    | Bsub -> Ok (Vnumber (Nfloat (float_of_int i -. f)), env)
    | Bmul -> Ok (Vnumber (Nfloat (float_of_int i *. f)), env)
    | Bdiv -> Ok (Vnumber (Nfloat (float_of_int i /. f)), env)
    | Bfldiv ->
      let _, q = Float.modf (float_of_int i /. f) in
      Ok (Vnumber (Nfloat q), env)
    | Bmod ->
      let fi = float_of_int i in
      let _, q = Float.modf (fi /. f) in
      Ok (Vnumber (Nfloat (fi -. (f *. q))), env)
    | Bexp -> Ok (Vnumber (Nfloat (Float.pow (float_of_int i) f)), env)
    | _ -> assert false (* call error *)
  end
  | Vnumber (Nfloat f1), Vnumber (Nfloat f2) -> begin
    match binop with
    | Badd -> Ok (Vnumber (Nfloat (f1 +. f2)), env)
    | Bsub -> Ok (Vnumber (Nfloat (f1 -. f2)), env)
    | Bmul -> Ok (Vnumber (Nfloat (f1 *. f2)), env)
    | Bdiv -> Ok (Vnumber (Nfloat (f1 /. f2)), env)
    | Bfldiv ->
      let _, q = Float.modf (f1 /. f2) in
      Ok (Vnumber (Nfloat q), env)
    | Bmod ->
      let _, q = Float.modf (f1 /. f2) in
      Ok (Vnumber (Nfloat (f1 -. (f2 *. q))), env)
    | Bexp -> Ok (Vnumber (Nfloat (Float.pow f1 f2)), env)
    | _ -> assert false (* call error *)
  end
  | _ -> assert false (* typing error *)

and interpret_bitwise_binop_expr binop ((loc1, _) as expr1) ((loc2, _) as expr2)
  env =
  let* v1, env = interpret_expr expr1 env in
  let* v2, env = interpret_expr expr2 env in
  let* _ =
    typecheck_expr
      (loc1, Ebinop ((loc1, Evalue v1), binop, (loc2, Evalue v2)))
      env
  in
  match (v1, v2) with
  | Vnumber (Ninteger i1), Vnumber (Ninteger i2) -> begin
    match binop with
    | Bland -> Ok (Vnumber (Ninteger (i1 land i2)), env)
    | Blor -> Ok (Vnumber (Ninteger (i1 lor i2)), env)
    | Blxor -> Ok (Vnumber (Ninteger (i1 lxor i2)), env)
    | Blsl -> Ok (Vnumber (Ninteger (i1 lsl i2)), env)
    | Blsr -> Ok (Vnumber (Ninteger (i1 lsr i2)), env)
    | _ -> assert false (* call error *)
  end
  | Vnumber (Nfloat f), Vnumber (Ninteger i) -> begin
    match binop with
    | Bland -> Ok (Vnumber (Ninteger (get_int f loc1 land i)), env)
    | Blor -> Ok (Vnumber (Ninteger (get_int f loc1 lor i)), env)
    | Blxor -> Ok (Vnumber (Ninteger (get_int f loc1 lxor i)), env)
    | Blsl -> Ok (Vnumber (Ninteger (get_int f loc1 lsl i)), env)
    | Blsr -> Ok (Vnumber (Ninteger (get_int f loc1 lsr i)), env)
    | _ -> assert false (* call error *)
  end
  | Vnumber (Ninteger i), Vnumber (Nfloat f) -> begin
    match binop with
    | Bland -> Ok (Vnumber (Ninteger (i land get_int f loc2)), env)
    | Blor -> Ok (Vnumber (Ninteger (i lor get_int f loc2)), env)
    | Blxor -> Ok (Vnumber (Ninteger (i lxor get_int f loc2)), env)
    | Blsl -> Ok (Vnumber (Ninteger (i lsl get_int f loc2)), env)
    | Blsr -> Ok (Vnumber (Ninteger (i lsr get_int f loc2)), env)
    | _ -> assert false (* call error *)
  end
  | Vnumber (Nfloat f1), Vnumber (Nfloat f2) -> begin
    match binop with
    | Bland ->
      Ok (Vnumber (Ninteger (get_int f1 loc1 land get_int f2 loc2)), env)
    | Blor -> Ok (Vnumber (Ninteger (get_int f1 loc1 lor get_int f2 loc2)), env)
    | Blxor ->
      Ok (Vnumber (Ninteger (get_int f1 loc1 lxor get_int f2 loc2)), env)
    | Blsl -> Ok (Vnumber (Ninteger (get_int f1 loc1 lsl get_int f2 loc2)), env)
    | Blsr -> Ok (Vnumber (Ninteger (get_int f1 loc1 lsr get_int f2 loc2)), env)
    | _ -> assert false (* call error *)
  end
  | _ -> assert false (* typing error *)

and interpret_rel_binop_expr binop ((loc1, _) as expr1) ((loc2, _) as expr2) env
    =
  let* v1, env = interpret_expr expr1 env in
  let* v2, env = interpret_expr expr2 env in
  let* _ =
    typecheck_expr
      (loc1, Ebinop ((loc1, Evalue v1), binop, (loc2, Evalue v2)))
      env
  in
  match (v1, v2) with
  | Vnumber (Ninteger i1), Vnumber (Ninteger i2) -> begin
    match binop with
    | Blt -> Ok (Vboolean (i1 < i2), env)
    | Ble -> Ok (Vboolean (i1 <= i2), env)
    | Bgt -> Ok (Vboolean (i1 > i2), env)
    | Bge -> Ok (Vboolean (i1 >= i2), env)
    | Beq -> Ok (Vboolean (i1 = i2), env)
    | Bneq -> Ok (Vboolean (i1 != i2), env)
    | _ -> assert false (* call error *)
  end
  | Vnumber (Nfloat f), Vnumber (Ninteger i) -> begin
    match binop with
    | Blt -> Ok (Vboolean (f < float_of_int i), env)
    | Ble -> Ok (Vboolean (f <= float_of_int i), env)
    | Bgt -> Ok (Vboolean (f > float_of_int i), env)
    | Bge -> Ok (Vboolean (f >= float_of_int i), env)
    | Beq -> Ok (Vboolean (f = float_of_int i), env)
    | Bneq -> Ok (Vboolean (f != float_of_int i), env)
    | _ -> assert false (* call error *)
  end
  | Vnumber (Ninteger i), Vnumber (Nfloat f) -> begin
    match binop with
    | Blt -> Ok (Vboolean (float_of_int i < f), env)
    | Ble -> Ok (Vboolean (float_of_int i <= f), env)
    | Bgt -> Ok (Vboolean (float_of_int i > f), env)
    | Bge -> Ok (Vboolean (float_of_int i >= f), env)
    | Beq -> Ok (Vboolean (float_of_int i = f), env)
    | Bneq -> Ok (Vboolean (float_of_int i != f), env)
    | _ -> assert false (* call error *)
  end
  | Vnumber (Nfloat f1), Vnumber (Nfloat f2) -> begin
    match binop with
    | Blt -> Ok (Vboolean (f1 < f2), env)
    | Ble -> Ok (Vboolean (f1 <= f2), env)
    | Bgt -> Ok (Vboolean (f1 > f2), env)
    | Bge -> Ok (Vboolean (f1 >= f2), env)
    | Beq -> Ok (Vboolean (f1 = f2), env)
    | Bneq -> Ok (Vboolean (f1 != f2), env)
    | _ -> assert false (* call error *)
  end
  | Vnil (), Vnil () -> begin
    match binop with
    | Beq -> Ok (Vboolean true, env)
    | Bneq -> Ok (Vboolean false, env)
    | _ -> assert false (* typing error *)
  end
  | Vboolean b1, Vboolean b2 -> begin
    match binop with
    | Beq -> Ok (Vboolean (b1 = b2), env)
    | Bneq -> Ok (Vboolean (b1 <> b2), env)
    | _ -> assert false (* typing error *)
  end
  | Vstring s1, Vstring s2 -> begin
    match binop with
    | Blt -> Ok (Vboolean (s1 < s2), env)
    | Ble -> Ok (Vboolean (s1 <= s2), env)
    | Bgt -> Ok (Vboolean (s1 > s2), env)
    | Bge -> Ok (Vboolean (s1 >= s2), env)
    | Beq -> Ok (Vboolean (s1 = s2), env)
    | Bneq -> Ok (Vboolean (s1 <> s2), env)
    | _ -> assert false
  end
  | v1, v2 when v1 <> v2 -> begin
    match binop with
    | Beq -> Ok (Vboolean false, env)
    | Bneq -> Ok (Vboolean true, env)
    | _ -> assert false (* typing error *)
  end
  | _ -> assert false (* todo: to be implemented *)

and interpret_str_binop_expr ((loc1, _) as expr1) ((loc2, _) as expr2) env =
  let* v1, env = interpret_expr expr1 env in
  let* v2, env = interpret_expr expr2 env in
  let* _ =
    typecheck_expr
      (loc1, Ebinop ((loc1, Evalue v1), Bddot, (loc2, Evalue v2)))
      env
  in
  begin
    match (v1, v2) with
    | Vstring s1, Vstring s2 -> Ok (Vstring (s1 ^ s2), env)
    | Vstring s, Vnumber (Ninteger i) -> Ok (Vstring (s ^ string_of_int i), env)
    | Vstring s, Vnumber (Nfloat f) -> Ok (Vstring (s ^ string_of_float f), env)
    | Vnumber (Ninteger i), Vstring s -> Ok (Vstring (string_of_int i ^ s), env)
    | Vnumber (Nfloat f), Vstring s -> Ok (Vstring (string_of_float f ^ s), env)
    | Vnumber (Ninteger i1), Vnumber (Ninteger i2) ->
      Ok (Vstring (string_of_int i1 ^ string_of_int i2), env)
    | Vnumber (Nfloat f1), Vnumber (Nfloat f2) ->
      Ok (Vstring (string_of_float f1 ^ string_of_float f2), env)
    | Vnumber (Ninteger i), Vnumber (Nfloat f) ->
      Ok (Vstring (string_of_int i ^ string_of_float f), env)
    | Vnumber (Nfloat f), Vnumber (Ninteger i) ->
      Ok (Vstring (string_of_float f ^ string_of_int i), env)
    | _ -> assert false (* typing error *)
  end

and interpret_prefixexp pexp env =
  match pexp with
  | PEvar v -> interpret_var v env
  | PEexp exp -> interpret_expr exp env
  | PEfunctioncall fc -> interpret_functioncall fc env

and interpret_var v env =
  match v with
  | VarName n ->
    let* v = env_result_check (Env.get_value n env) in
    Ok (v, env)
  | VarTableField (pexp, exp) -> (
    let* _ = typecheck_var (VarTableField (pexp, exp)) env in
    let* v, env = interpret_prefixexp pexp env in
    let* idx, env = interpret_expr exp env in
    match v with
    | Vtable (_i, tbl) -> begin
      match Table.get get_int_value_opt idx tbl with
      | None -> Ok (Vnil (), env)
      | Some v -> Ok (v, env)
    end
    | _ -> Ok (Vnil (), env) )

and interpret_field field env =
  match field with
  | Fexp exp ->
    let* v, env = interpret_expr exp env in
    Ok ((Vnil (), v), env)
  | Fname (n, exp) ->
    let* v, env = interpret_expr exp env in
    Ok ((Vstring n, v), env)
  | Fcol (exp1, exp2) ->
    let* v1, env = interpret_expr exp1 env in
    let* v2, env = interpret_expr exp2 env in
    Ok ((v1, v2), env)

and tableconstructor tbl idx fl env =
  let tbl_add_rec id v tbl idx fl env =
    match id with
    | Vnil () ->
      let t = Table.add get_int_value_opt (Vnumber (Ninteger idx)) v tbl in
      tableconstructor t (idx + 1) fl env
    | v_idx ->
      let t = Table.add get_int_value_opt v_idx v tbl in
      tableconstructor t idx fl env
  in
  match fl with
  | [] -> Ok (tbl, env)
  | [ f ] ->
    let* (v_idx, v_val), env = interpret_field f env in
    begin
      match v_val with
      | VfunctionReturn vl ->
        let tbl, _i, env =
          List.fold_left
            (fun (t, id, ev) v ->
              match v_idx with
              | Vnil () ->
                ( Table.add get_int_value_opt (Vnumber (Ninteger id)) v t
                , id + 1
                , ev )
              | v_idx -> (Table.add get_int_value_opt v_idx v t, id, ev) )
            (tbl, idx, env) vl
        in
        Ok (tbl, env)
      | v -> begin
        match v_idx with
        | Vnil () ->
          Ok (Table.add get_int_value_opt (Vnumber (Ninteger idx)) v tbl, env)
        | v_idx -> Ok (Table.add get_int_value_opt v_idx v tbl, env)
      end
    end
  | f :: fl ->
    let* (v_idx, v_val), env = interpret_field f env in
    begin
      match v_val with
      | VfunctionReturn vl -> begin
        match vl with
        | [] -> tbl_add_rec v_idx (Vnil ()) tbl idx fl env
        | v :: _vl -> tbl_add_rec v_idx v tbl idx fl env
      end
      | v -> tbl_add_rec v_idx v tbl idx fl env
    end

and interpret_expr (loc, expr) env =
  match expr with
  | Evalue
      ( ( Vnil ()
        | Vboolean _ | Vnumber _ | Vstring _ | Vfunction _ | VfunctionStdLib _
        | VfunctionReturn _ | Vtable _ ) as v ) ->
    Ok (v, env)
  | Eunop (Unot, ((l, _) as e)) ->
    let* v, env = interpret_expr e env in
    let* _ = typecheck_expr (loc, Eunop (Unot, (l, Evalue v))) env in
    begin
      match v with
      | Vnil () -> Ok (Vboolean true, env)
      | Vboolean b -> Ok (Vboolean (not b), env)
      | _ -> Ok (Vboolean false, env)
    end
  | Eunop (Uminus, ((l, _) as e)) ->
    let* v, env = interpret_expr e env in
    let* _ = typecheck_expr (loc, Eunop (Uminus, (l, Evalue v))) env in
    begin
      match v with
      | Vnumber (Ninteger i) -> Ok (Vnumber (Ninteger (-i)), env)
      | Vnumber (Nfloat f) -> Ok (Vnumber (Nfloat (-.f)), env)
      | Vstring s ->
        let v = number_of_string (Some l) s in
        begin
          match v with
          | Vnumber (Ninteger i) -> Ok (Vnumber (Ninteger (-i)), env)
          | Vnumber (Nfloat f) -> Ok (Vnumber (Nfloat (-.f)), env)
          | _ -> assert false (* call error *)
        end
      | _ -> assert false (* typing error *)
    end
  | Eunop (Usharp, ((l, _) as e)) ->
    let* v, env = interpret_expr e env in
    let* _ = typecheck_expr (loc, Eunop (Usharp, (l, Evalue v))) env in
    begin
      match v with
      | Vstring s -> Ok (Vnumber (Ninteger (String.length s)), env)
      | Vtable (_i, t) ->
        Ok (Vnumber (Ninteger (Table.len t)), env)
        (* todo: not correct: Table.len isn't exactly Table.border *)
      | _ -> assert false (* typing error *)
    end
  | Eunop (Ulnot, ((l, _) as e)) ->
    let* v, env = interpret_expr e env in
    let* _ = typecheck_expr (loc, Eunop (Ulnot, (l, Evalue v))) env in
    begin
      match v with
      | Vnumber (Ninteger i) -> Ok (Vnumber (Ninteger (lnot i)), env)
      | Vnumber (Nfloat f) -> Ok (Vnumber (Ninteger (lnot (get_int f loc))), env)
      | _ -> assert false (* typing error *)
    end
  | Ebinop (e1, ((Band | Bor) as op), e2) -> interpret_bbinop_expr op e1 e2 env
  | Ebinop (e1, Bddot, e2) -> interpret_str_binop_expr e1 e2 env
  | Ebinop (e1, ((Badd | Bsub | Bmul | Bdiv | Bfldiv | Bmod | Bexp) as op), e2)
    ->
    interpret_arith_binop_expr op e1 e2 env
  | Ebinop (e1, ((Bland | Blor | Blxor | Blsl | Blsr) as op), e2) ->
    interpret_bitwise_binop_expr op e1 e2 env
  | Ebinop (e1, ((Blt | Ble | Bgt | Bge | Beq | Bneq) as op), e2) ->
    interpret_rel_binop_expr op e1 e2 env
  | Evariadic -> Ok (Vnil (), env)
  | Efunctiondef fb -> Ok (Vfunction (Random.bits32 (), fb, env), env)
  | Eprefix pexp -> interpret_prefixexp pexp env
  | Etableconstructor fl ->
    let* table, env = tableconstructor Table.empty 1 fl env in
    Ok (Vtable (Random.bits32 (), table), env)

and set_var v value env =
  let rec var_of_prefixexp pexp env =
    match pexp with
    | PEvar v -> v
    | PEfunctioncall (FCpreargs (pexp, _a)) -> var_of_prefixexp pexp env
    | PEfunctioncall (FCprename (pexp, _s, _a)) -> var_of_prefixexp pexp env
    | PEexp _exp -> assert false (* todo *)
  in
  match v with
  | VarName n ->
    let* () = env_result_check (Env.update_value n value env) in
    Ok env
  | VarTableField (pexp, exp) -> (
    let* _ = typecheck_var (VarTableField (pexp, exp)) env in
    let* t, env = interpret_prefixexp pexp env in
    let* idx, env = interpret_expr exp env in
    match t with
    | Vtable (i, tbl) ->
      let tbl = Table.add get_int_value_opt idx value tbl in
      let vtbl = Vtable (i, tbl) in
      let v = var_of_prefixexp pexp env in
      set_var v vtbl env
    | _ ->
      error None
        (Format.sprintf "Typing error: attempt to index a non table value") )

and to_vall el env =
  List.fold_left
    (fun acc ((l, _e) as exp) ->
      let vl, e = Result.get_ok acc in
      let* v, e = interpret_expr exp e in
      Ok (vl @ [ (l, v) ], e) )
    (Ok ([], env))
    el

and lists_assign vl vall env =
  begin
    match (vl, vall) with
    | [], [] | [], _ -> Ok env
    | vl, [] ->
      List.fold_left
        (fun acc v ->
          let e = Result.get_ok acc in
          set_var v (Vnil ()) e )
        (Ok env) vl
    | v :: vl, [ (l, va) ] -> (
      match va with
      | VfunctionReturn vall -> begin
        match vall with
        | [] -> set_var v (Vnil ()) env
        | va :: vall ->
          let* env = set_var v va env in
          let vall = List.map (fun v -> (l, v)) vall in
          lists_assign vl vall env
      end
      | va ->
        let* env = set_var v va env in
        lists_assign vl [] env )
    | v :: vl, (_l, va) :: tl -> (
      match va with
      | VfunctionReturn vall -> begin
        match vall with
        | [] -> set_var v (Vnil ()) env
        | va :: _vall ->
          let* env = set_var v va env in
          lists_assign vl tl env
      end
      | va ->
        let* env = set_var v va env in
        lists_assign vl tl env )
  end

(* todo: local name attrib (const/close) support *)
and lists_lassign nal vall env =
  begin
    match (nal, vall) with
    | [], [] | [], _ -> Ok env
    | nal, [] ->
      List.fold_left
        (fun acc (n, _on) ->
          let e = Result.get_ok acc in
          Env.add_value n (Vnil ()) e )
        (Ok env) nal
    | (n, _on) :: vl, [ (l, va) ] -> (
      match va with
      | VfunctionReturn vall -> begin
        match vall with
        | [] -> Env.add_value n (Vnil ()) env
        | va :: vall ->
          let* env = Env.add_value n va env in
          let vall = List.map (fun v -> (l, v)) vall in
          lists_lassign vl vall env
      end
      | Vfunction (_i, _bl, cl_env) as f ->
        let* () = env_result_check (Env.update_value n f cl_env) in
        lists_lassign vl [] env
      | va ->
        let* env = Env.add_value n va env in
        lists_lassign vl [] env )
    | (n, _on) :: vl, (_l, va) :: tl -> (
      match va with
      | VfunctionReturn vall -> begin
        match vall with
        | [] -> Env.add_value n (Vnil ()) env
        | va :: _vall ->
          let* env = Env.add_value n va env in
          lists_lassign vl tl env
      end
      | va ->
        let* env = Env.add_value n va env in
        lists_lassign vl tl env )
  end

(* todo: variadic function (PLvariadic, PLlist some) *)
and lists_args pl vall env =
  match pl with
  | PLvariadic _ -> assert false
  | PLlist (_nl, Some _) -> assert false
  | PLlist (nl, None) ->
    let vl = List.map (fun n -> (n, None)) nl in
    lists_lassign vl vall env

and interpret_fct value el env =
  match value with
  | Vfunction (i, (pl, b), cl_env) as closure -> begin
    try
      let* vall, env = to_vall el env in
      let* cl_env = lists_args pl vall cl_env in
      let* cl_env = interpret_block b cl_env in
      let closure = Vfunction (i, (pl, b), cl_env) in
      Ok (closure, VfunctionReturn [], env)
    with Return_catch (el, cl_env) ->
      begin
        match el with
        | [] -> Ok (closure, VfunctionReturn [], env)
        | [ e ] ->
          let* v, cl_env = interpret_expr e cl_env in
          (* shortcut: directly consider it's a value *)
          let closure = Vfunction (i, (pl, b), cl_env) in
          Ok (closure, v, env)
        | el ->
          let* vll, cl_env = to_vall el cl_env in
          let vl = List.map (fun (_l, v) -> v) vll in
          let closure = Vfunction (i, (pl, b), cl_env) in
          Ok (closure, VfunctionReturn vl, env)
      end
  end
  | VfunctionStdLib (i, fct) ->
    let* vall, env = to_vall el env in
    let vall = List.map (fun (_l, v) -> v) vall in
    begin
      try
        let ret = fct vall in
        Ok (VfunctionStdLib (i, fct), VfunctionReturn ret, env)
      with
      | Lua_stdlib_common.Stdlib_typing_error msg ->
        error None (Format.sprintf "Typing error: %s" msg)
      | Lua_stdlib_common.Stdlib_error msg -> error None msg
    end
  | VfunctionReturn vl -> begin
    match vl with
    | [] -> error None "Typing error: attempt to call a nil value"
    | [ v ] -> interpret_fct v el env
    | v :: _vl -> interpret_fct v el env
  end
  | Vnil _ -> error None "Typing error: attempt to call a nil value"
  | _ -> assert false

and interpret_functioncall fc env =
  let* _ = typecheck_functioncall fc env in
  match fc with
  | FCpreargs (PEvar (VarName v), Aexpl el) ->
    let* value = env_result_check (Env.get_value v env) in
    let* closure, return, env = interpret_fct value el env in
    let* () = env_result_check (Env.update_value v closure env) in
    Ok (return, env)
  | FCpreargs (PEvar (VarTableField (pexp, exp)), Aexpl el) ->
    let* t, env = interpret_prefixexp pexp env in
    let* idx, env = interpret_expr exp env in
    begin
      match t with
      | Vtable (_i, tbl) -> begin
        match Table.get get_int_value_opt idx tbl with
        | None -> assert false
        | Some value ->
          let* _closure, return, env = interpret_fct value el env in
          Ok (return, env)
      end
      | _ -> assert false (* typing error *)
    end
  | FCpreargs (PEexp e, Aexpl el) ->
    let* value, env = interpret_expr e env in
    let* _closure, return, env = interpret_fct value el env in
    Ok (return, env)
  | FCpreargs (PEfunctioncall fc, Aexpl el) ->
    let* value, env = interpret_functioncall fc env in
    let* _closure, return, env = interpret_fct value el env in
    Ok (return, env)
  | FCprename (PEvar (VarName v), name, Aexpl el) ->
    let* value = env_result_check (Env.get_value v env) in
    begin
      match value with
      | Vtable (_i, tbl) -> begin
        match Table.get get_int_value_opt (Vstring name) tbl with
        | None -> assert false
        | Some value ->
          let* _closure, return, env = interpret_fct value el env in
          Ok (return, env)
      end
      | _ -> error None "Typing error: attempt to access a non table field"
    end
  | _ -> assert false (* TODO: pattern matching non exhaustive *)

and interpret_stmt stmt env : _ result =
  match stmt with
  | Sempty -> Ok env
  | Sassign (vl, el) ->
    let* vall, env = to_vall el env in
    lists_assign vl vall env
  | SassignLocal (nal, el) ->
    let* vall, env = to_vall el env in
    lists_lassign nal vall env
  | Sbreak -> raise (Break_catch env)
  | Sreturn el -> raise (Return_catch (el, env))
  | Slabel _ -> Ok env
  | Sgoto n -> raise (Goto_catch (Label n, env))
  | Sblock b -> interpret_block b env
  | Swhile (e, b) ->
    (* Doc: The condition expression of a control structure can return any value.
       Both false and nil test false. *)
    let* cond, env = interpret_expr e env in
    begin
      match cond with
      | Vboolean false | Vnil () -> Ok env
      | _ -> (
        try
          let* env = interpret_block b env in
          interpret_stmt (Swhile (e, b)) env
        with Break_catch env -> Ok env )
    end
  | Srepeat (b, e) -> begin
    try
      let* env = interpret_block b env in
      let* cond, env = interpret_expr e env in
      match cond with
      | Vboolean false | Vnil () -> interpret_stmt (Srepeat (b, e)) env
      | _ -> Ok env
    with Break_catch env -> Ok env
  end
  | Sif (e, b, ebl, ob) ->
    let rec interpret_elseif ebl env =
      match ebl with
      | [] -> Ok (None, env)
      | (e, b) :: tl ->
        let* cond, env = interpret_expr e env in
        begin
          match cond with
          | Vboolean false | Vnil () -> interpret_elseif tl env
          | _ ->
            let* env = interpret_block b env in
            Ok (Some (), env)
        end
    in
    let* cond, env = interpret_expr e env in
    begin
      match cond with
      | Vboolean false | Vnil () ->
        let* opt, env = interpret_elseif ebl env in
        begin
          match opt with
          | Some () -> Ok env
          | None -> begin
            match ob with Some b -> interpret_block b env | None -> Ok env
          end
        end
      | _ -> interpret_block b env
    end
  | Sfor (n, e1, e2, oe, b) ->
    let* () = typecheck_stmt (Sfor (n, e1, e2, oe, b)) env in
    let init_val ((l, _e) as expr) env =
      let* v, env = interpret_expr expr env in
      match v with
      | Vnumber (Ninteger i) -> Ok (Vnumber (Ninteger i), env)
      | Vnumber (Nfloat f) -> Ok (Vnumber (Nfloat f), env)
      | Vstring s -> begin
        match float_of_string_opt s with
        | Some f -> Ok (Vnumber (Nfloat f), env)
        | None ->
          error (Some l)
            (Format.sprintf
               "Typing error: bad 'for' limit (number expected, got string \
                '%s' without float representation)"
               s )
      end
      | _ -> assert false (* typing error *)
    in
    let cond_expr loc ival limit step =
      let op =
        match step with
        | Vnumber (Ninteger i) -> if i >= 0 then Ble else Bge
        | Vnumber (Nfloat f) -> if f >= 0. then Ble else Bge
        | _ -> assert false (* call error *)
      in
      (loc, Ebinop ((loc, Evalue ival), op, (loc, Evalue limit)))
    in
    let incr_cnt loc ival step env =
      interpret_expr
        (loc, Ebinop ((loc, Evalue ival), Badd, (loc, Evalue step)))
        env
    in
    let l1, _e1 = e1 in
    let* ival, env = init_val e1 env in
    let* env = Env.add_value n ival env in
    let* limit, env = init_val e2 env in
    let* step, env =
      match oe with
      | Some e -> init_val e env
      | None -> Ok (Vnumber (Ninteger 1), env)
    in
    let cexpr = cond_expr l1 ival limit step in
    let* cond, env = interpret_expr cexpr env in
    begin
      match cond with
      | Vboolean false | Vnil () -> Ok env
      | _ -> (
        try
          let* env = interpret_block b env in
          let* env = Env.add_value n ival env in
          (* control var must be restored *)
          let* ival, _ = incr_cnt l1 ival step env in
          interpret_stmt (Sfor (n, (l1, Evalue ival), e2, oe, b)) env
        with Break_catch env -> Ok env )
    end
  | Siterator (nl, el, b) ->
    let* () = typecheck_stmt (Siterator (nl, el, b)) env in
    let loc, _e = List.nth el 0 in
    let* vl, env =
      List.fold_left
        (fun acc ex ->
          let vl, ev = Result.get_ok acc in
          let* v, ev = interpret_expr ex ev in
          match v with
          | VfunctionReturn l -> Ok (vl @ l, ev)
          | v -> Ok (vl @ [ v ], ev) )
        (Ok ([], env))
        el
    in
    begin
      match List.length vl with
      | 1 ->
        (* Stateful iterator *)
        let iter cl env =
          try
            let* env = interpret_block b env in
            interpret_stmt (Siterator (nl, [ (loc, Evalue cl) ], b)) env
          with Break_catch env -> Ok env
        in
        let ctrl_value = List.nth vl 0 in
        begin
          match ctrl_value with
          | Vfunction (_i, (_pl, _bl), cl_env) as closure -> (
            let* closure, v, _cl_env = interpret_fct closure [] cl_env in
            match v with
            | Vnil () -> Ok env (* stop condition *)
            | VfunctionReturn vl -> begin
              match vl with
              | [] -> Ok env
              | v :: tl ->
                let* env = Env.add_value (List.nth nl 0) v env in
                let ni = ref 0 in
                let* env =
                  List.fold_left
                    (fun acc v ->
                      let ev = Result.get_ok acc in
                      ni := !ni + 1;
                      match List.nth_opt nl !ni with
                      | None -> Ok ev
                      | Some n -> Env.add_value n v ev )
                    (Ok env) tl
                in
                iter closure env
            end
            | v ->
              let* env = Env.add_value (List.nth nl 0) v env in
              iter closure env )
          | _ ->
            error None
              "Typing error: bad 'for iterator' stateful construction \
               (iterator function in 'in' argument does not return a closure)"
        end
      | n when n < 3 ->
        error None
          "Typing error: bad 'for iterator' stateless construction (bad \
           element number in 'in' argument)"
      | _ ->
        (* Stateless iterator *)
        (* 4 values: iterator function, state, an initial value for the control variable, and a closing value. *)
        let iterator_func = List.nth vl 0 in
        let state = List.nth vl 1 in
        let ctrl_var = List.nth vl 2 in
        begin
          match ctrl_var with
          | ctrl_var -> (
            let iterator_func_param =
              [ (loc, Evalue state); (loc, Evalue ctrl_var) ]
            in
            let* _closure, v, env =
              interpret_fct iterator_func iterator_func_param env
            in
            let* ctrl_var, env =
              match v with
              | VfunctionReturn vl -> begin
                match vl with
                | [] -> Ok (Vnil (), env)
                | v :: tl ->
                  let* env = Env.add_value (List.nth nl 0) v env in
                  let ni = ref 0 in
                  let* env =
                    List.fold_left
                      (fun acc v ->
                        let ev = Result.get_ok acc in
                        ni := !ni + 1;
                        match List.nth_opt nl !ni with
                        | None -> Ok ev
                        | Some n -> Env.add_value n v ev )
                      (Ok env) tl
                  in
                  Ok (v, env)
              end
              | v ->
                let* env = Env.add_value (List.nth nl 0) v env in
                Ok (v, env)
            in
            match ctrl_var with
            | Vnil () -> Ok env (* stop condition *)
            | ctrl_var -> (
              try
                let* env = interpret_block b env in
                interpret_stmt
                  (Siterator
                     ( nl
                     , [ (loc, Evalue iterator_func)
                       ; (loc, Evalue state)
                       ; (loc, Evalue ctrl_var)
                       ]
                     , b ) )
                  env
              with Break_catch env -> Ok env ) )
        end
    end
  (* | Sfunction (_n, _fb) -> env *)
  (* | SfunctionLocal (_n, _fb) -> env *)
  | SfunctionCall fc ->
    let* _v, env = interpret_functioncall fc env in
    Ok env

and interpret_block b env =
  List.fold_left
    (fun acc stmt ->
      let e = Result.get_ok acc in
      interpret_stmt stmt e )
    (Ok env) b

let rec run ?(pt = Begin) chunk env =
  try
    let bl = block_from_pointer pt chunk in
    interpret_block bl env
  with
  | Goto_catch (label, env) -> run ~pt:label chunk env
  | Return_catch (_el, env) -> Ok env
