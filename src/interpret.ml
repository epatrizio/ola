open Ast
open Ast.Value
open Evaluator
open Syntax
open Typer

let () = Random.self_init ()

type block_pointer =
  | Begin
  | Last
  | Label of string

exception Goto_catch of block_pointer * Ast.Value.t Env.t

exception Break_catch of Ast.Value.t Env.t

exception Return_catch of expr list * Ast.Value.t Env.t

exception Interpretation_error of location option * string

let error loc_opt message = raise (Interpretation_error (loc_opt, message))

let rec block_from_pointer pt stmt_list =
  match (pt, stmt_list) with
  | Begin, _ -> stmt_list
  | Last, ([] | [ _ ]) -> stmt_list
  | Last, _stmt :: tl -> block_from_pointer Last tl
  | Label l, [] -> error None ("no visible label for <goto> " ^ l)
  | Label l, Slabel n :: tl when l = n -> tl
  | Label l, _stmt :: tl -> block_from_pointer (Label l) tl

let rec interpret_bbinop_expr binop expr1 expr2 env =
  let* v1, env = interpret_expr expr1 env in
  (* let* v2, env = interpret_expr expr2 env in
  let v = eval_bbinop binop v1 v2 in
  Ok (v, env) *)
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

and interpret_prefixexp pexp env =
  match pexp with
  | PEvar v -> interpret_var v env
  | PEexp exp ->
    let* v, env = interpret_expr exp env in
    begin match v with
    | VfunctionReturn (v :: _tl) -> Ok (VfunctionReturn [ v ], env)
    | _ -> Ok (v, env)
    end
  | PEfunctioncall fc -> interpret_functioncall fc env

and interpret_var v env =
  match v with
  | VarName n ->
    let* v = Env.get_value n env in
    Ok (v, env)
  | VarTableField (pexp, ((l, _) as exp)) -> (
    let* t, env = interpret_prefixexp pexp env in
    let* idx, env = interpret_expr exp env in
    let* _ =
      typecheck_var (VarTableField (PEexp (l, Evalue t), (l, Evalue idx))) env
    in
    let idx = Eval_utils.integer_of_float_value idx in
    match t with
    | VfunctionReturn (Vtable t :: _) | Vtable t -> begin
      match LuaTable.get idx t with
      | None -> index_metamechanism idx (Vtable t) env
      | Some v -> Ok (v, env)
    end
    | _ -> Ok (Vnil (), env) )

and index_metamechanism idx tbl env =
  match tbl with
  | Vtable t -> begin
    match LuaTable.get_metatable t with
    | Some mt -> begin
      match LuaTable.get (Vstring "__index") mt with
      | Some v -> begin
        match v with
        | Vtable t as tbl -> begin
          match LuaTable.get idx t with
          | None -> index_metamechanism idx tbl env
          | Some v -> Ok (v, env)
        end
        | Vfunction (_i, _pb, _env) as f ->
          let arr = (empty_location (), Evalue tbl) in
          let key = (empty_location (), Evalue idx) in
          let* _, v, _env = interpret_fct f [ arr; key ] _env in
          Ok (v, env)
        | _ ->
          error None
            "metatable.__index: attempt to index a non table or function value"
      end
      | None -> Ok (Vnil (), env)
    end
    | None -> Ok (Vnil (), env)
  end
  | _ -> assert false

and newindex_metamechanism idx value tbl env =
  let tbl_add idx value t env =
    let t = LuaTable.add idx value t in
    Ok (Vtable t, env)
  in
  match tbl with
  | Vtable t -> begin
    match LuaTable.get_metatable t with
    | Some mt -> begin
      match LuaTable.get (Vstring "__newindex") mt with
      | Some v -> begin
        match v with
        | Vtable _ -> assert false (* TODO *)
        | Vfunction (_i, _pb, _env) as f ->
          if LuaTable.key_exists idx t then tbl_add idx value t env
          else
            let arr = (empty_location (), Evalue tbl) in
            let key = (empty_location (), Evalue idx) in
            let value = (empty_location (), Evalue value) in
            let* _, _v, _env = interpret_fct f [ arr; key; value ] _env in
            Ok (Vtable t, env)
        | _ ->
          error None
            "metatable.__newindex: attempt to index a non table or function \
             value"
      end
      | None -> tbl_add idx value t env
    end
    | None -> tbl_add idx value t env
  end
  | _ -> assert false

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
  let field_handler ~is_last f env =
    let add_field tbl v_idx v_val =
      match v_idx with
      | Vnil () ->
        incr idx;
        LuaTable.add (Vnumber (Ninteger !idx)) v_val tbl
      | v_idx -> LuaTable.add v_idx v_val tbl
    in
    let* (v_idx, v_val), env = interpret_field f env in
    match v_val with
    | VfunctionReturn [] | Vvariadic [] ->
      let tbl = add_field tbl v_idx (Vnil ()) in
      Ok (tbl, env)
    | VfunctionReturn (v :: vl) | Vvariadic (v :: vl) ->
      let tbl =
        if is_last then
          List.fold_left (fun tbl v -> add_field tbl v_idx v) tbl (v :: vl)
        else add_field tbl v_idx v
      in
      Ok (tbl, env)
    | v_val ->
      let tbl = add_field tbl v_idx v_val in
      Ok (tbl, env)
  in
  match fl with
  | [] -> Ok (tbl, env)
  | [ f ] ->
    let* tbl, env = field_handler ~is_last:true f env in
    Ok (tbl, env)
  | f :: fl ->
    let* tbl, env = field_handler ~is_last:false f env in
    tableconstructor tbl idx fl env

and interpret_expr (loc, expr) env =
  match expr with
  | Evalue
      ( ( Vnil ()
        | Vboolean _ | Vnumber _ | Vstring _ | Vvariadic _ | Vfunction _
        | VfunctionStdLib _ | VfunctionReturn _ | Vtable _ ) as v ) ->
    Ok (v, env)
  | Eunop (unop, e) ->
    let* v, env = interpret_expr e env in
    eval_unop unop (loc, v) env
  | Ebinop (e1, ((Band | Bor) as op), e2) -> interpret_bbinop_expr op e1 e2 env
  | Ebinop (e1, binop, e2) ->
    let* v1, env = interpret_expr e1 env in
    let* v2, env = interpret_expr e2 env in
    eval_binop binop (loc, v1) (loc, v2) env
  | Evariadic ->
    let* v = Env.get_value "vararg" env in
    let* _ = typecheck_variadic v in
    Ok (v, env)
  | Efunctiondef fb -> Ok (Vfunction (Random.bits32 (), fb, env), env)
  | Eprefix pexp -> interpret_prefixexp pexp env
  | Etableconstructor fl ->
    let idx = ref 0 in
    let* table, env = tableconstructor LuaTable.empty idx fl env in
    Ok (Vtable table, env)

and set_var v value env =
  let rec var_of_prefixexp pexp env =
    match pexp with
    | PEvar v -> v
    | PEfunctioncall (FCpreargs (pexp, _a)) -> var_of_prefixexp pexp env
    | PEfunctioncall (FCprename (pexp, _s, _a)) -> var_of_prefixexp pexp env
    | PEexp (_loc, Eprefix pexp) -> var_of_prefixexp pexp env
    | PEexp (loc, _) ->
      error (Some loc) (Format.sprintf "Typing error: var_of_prefixexp PEexp")
    (* WIP *)
  in
  match v with
  | VarName n -> Env.update_value n value env
  | VarTableField (pexp, ((l, _) as exp)) -> (
    let* t, env = interpret_prefixexp pexp env in
    let* idx, env = interpret_expr exp env in
    let* _ =
      typecheck_var ~strict:true
        (VarTableField (PEexp (l, Evalue t), (l, Evalue idx)))
        env
    in
    let idx = Eval_utils.integer_of_float_value idx in
    match t with
    | Vtable t as tbl ->
      if value = Vnil () then
        let t = LuaTable.remove idx t in
        let v = var_of_prefixexp pexp env in
        set_var v (Vtable t) env
      else
        let* tbl, env = newindex_metamechanism idx value tbl env in
        let v = var_of_prefixexp pexp env in
        set_var v tbl env
    | _ -> assert false (* typing error *) )

and to_vall el env =
  List.fold_left
    (fun acc ((l, _e) as exp) ->
      let vl, e = Result.get_ok acc in
      let* v, e = interpret_expr exp e in
      Ok (vl @ [ (l, v) ], e) )
    (Ok ([], env))
    el

and lists_assign ?(is_local = false) vl vall env =
  let var_handler is_local var value env =
    if is_local then
      let name = match var with VarName name -> name | _ -> assert false in
      Env.add_value name value env
    else
      let* () = set_var var value env in
      Ok env
  in
  let assign_handler is_local var value vl tl env =
    if is_local then
      let* env = var_handler is_local var value env in
      lists_assign ~is_local vl tl env
    else
      let* env = lists_assign ~is_local vl tl env in
      var_handler is_local var value env
  in
  begin match (vl, vall) with
  | [], [] | [], _ -> Ok env
  | vl, [] ->
    List.fold_left
      (fun acc v ->
        let env = Result.get_ok acc in
        var_handler is_local v (Vnil ()) env )
      (Ok env) vl
  | v :: vl, [ (l, va) ] -> (
    match va with
    | VfunctionReturn vall | Vvariadic vall -> begin
      match vall with
      | [] -> var_handler is_local v (Vnil ()) env
      | va :: vall ->
        let vall = List.map (fun v -> (l, v)) vall in
        assign_handler is_local v va vl vall env
    end
    | Vfunction (_i, _bl, cl_env) as f ->
      (* wip *)
      if is_local then
        let name = match v with VarName name -> name | _ -> assert false in
        let* () = Env.update_value name f cl_env in
        lists_assign ~is_local vl [] env
      else assign_handler is_local v f vl [] env
    | va -> assign_handler is_local v va vl [] env )
  | v :: vl, (_l, va) :: tl -> (
    match va with
    | VfunctionReturn vall | Vvariadic vall -> begin
      match vall with
      | [] -> var_handler is_local v (Vnil ()) env (*set_var v (Vnil ()) env*)
      | va :: _vall -> assign_handler is_local v va vl tl env
    end
    | va -> assign_handler is_local v va vl tl env )
  end

and lists_args pl vall env =
  let vall_to_vvariadic vall cut_at_n =
    let _, vl = List.split vall in
    let _, vl = Utils.cut_list_at vl cut_at_n in
    Vvariadic vl
  in
  match pl with
  | PLvariadic ->
    let env = Env.add_local_force "vararg" (vall_to_vvariadic vall 0) env in
    Ok env
  | PLlist (nl, is_variadic) ->
    let env =
      if is_variadic then
        Env.add_local_force "vararg"
          (vall_to_vvariadic vall (List.length nl))
          env
      else env
    in
    let vl = List.map (fun n -> VarName n) nl in
    lists_assign ~is_local:true vl vall env

and interpret_fct value el env =
  let* _ = typecheck_function value in
  match value with
  | Vfunction (i, (pl, b), cl_env) as closure -> begin
    try
      let* vall, env = to_vall el env in
      let* cl_env = lists_args pl vall cl_env in
      let* cl_env = interpret_block b cl_env in
      let closure = Vfunction (i, (pl, b), cl_env) in
      Ok (closure, VfunctionReturn [], env)
    with Return_catch (el, cl_env) ->
      begin match el with
      | [] -> Ok (closure, VfunctionReturn [], env)
      | [ e ] ->
        let* v, cl_env = interpret_expr e cl_env in
        let closure = Vfunction (i, (pl, b), cl_env) in
        (* shortcut: directly consider it's a value instead of VfunctionReturn [ v ] *)
        (* Nb. VfunctionReturn [] != Vnil () *)
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
    begin try
      let ret, env = fct vall env in
      match ret with
      | [ v ] ->
        (* shortcut: directly consider it's a value *)
        Ok (VfunctionStdLib (i, fct), v, env)
      | _ -> Ok (VfunctionStdLib (i, fct), VfunctionReturn ret, env)
    with
    | Lua_stdlib_common.Stdlib_typing_error msg ->
      error None (Format.sprintf "Typing error: %s" msg)
    | Lua_stdlib_common.Stdlib_error msg -> error None msg
    end
  | VfunctionReturn vl -> begin
    match vl with
    | v :: _ -> interpret_fct v el env
    | _ -> assert false (* typing error *)
  end
  | _ -> assert false

and interpret_functioncall fc env =
  match fc with
  | FCpreargs (PEvar (VarName v), Aexpl el) ->
    let* value = Env.get_value v env in
    let* closure, return, env = interpret_fct value el env in
    let* () = Env.update_value v closure env in
    Ok (return, env)
  | FCpreargs (PEvar (VarTableField (pexp, exp)), Aexpl el) ->
    let* t, env = interpret_prefixexp pexp env in
    let* idx, env = interpret_expr exp env in
    let idx = Eval_utils.integer_of_float_value idx in
    begin match t with
    | Vtable t -> begin
      match LuaTable.get idx t with
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
  | FCprename ((PEvar (VarName v) as var), name, Aexpl el) ->
    let* value = Env.get_value v env in
    begin match value with
    | Vtable t as tbl ->
      let name = Vstring name in
      let* value, env =
        match LuaTable.get name t with
        | None -> index_metamechanism name tbl env
        | Some value -> Ok (value, env)
      in
      (* colon(:) syntactic sugar: self (first arg) *)
      let self = (empty_location (), Eprefix var) in
      let el = self :: el in
      let* _closure, return, env = interpret_fct value el env in
      Ok (return, env)
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
    let vl = List.map (fun (name, _) -> VarName name) nal in
    let* vall, env = to_vall el env in
    lists_assign ~is_local:true vl vall env
  | Sbreak -> raise (Break_catch env)
  | Sreturn el -> raise (Return_catch (el, env))
  | Slabel _ -> Ok env
  | Sgoto n -> raise (Goto_catch (Label n, env))
  | Sblock b -> interpret_block b env
  | Swhile (e, b) ->
    (* Doc: The condition expression of a control structure can return any value.
       Both false and nil test false. *)
    let* cond, env = interpret_expr e env in
    begin match cond with
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
        begin match cond with
        | Vboolean false | Vnil () -> interpret_elseif tl env
        | _ ->
          let* env = interpret_block b env in
          Ok (Some (), env)
        end
    in
    let* cond, env = interpret_expr e env in
    begin match cond with
    | Vboolean false | Vnil () ->
      let* opt, env = interpret_elseif ebl env in
      begin match opt with
      | Some () -> Ok env
      | None -> begin
        match ob with Some b -> interpret_block b env | None -> Ok env
      end
      end
    | _ -> interpret_block b env
    end
  | Sfor (n, e1, e2, oe, b) ->
    let init_val ((l, _e) as expr) env =
      let* v, env = interpret_expr expr env in
      let v = Eval_utils.number_of_string (Some l) v in
      let* _ = typecheck_for_ctrl_expr (l, Evalue v) env in
      Ok (v, env)
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
    begin match cond with
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
    let evl = List.map (fun v -> (loc, Evalue v)) vl in
    let* _ = typecheck_iterator_ctrl_el evl env in
    begin match vl with
    | [ ctrl_value ] ->
      (* Stateful iterator *)
      let iter cl env =
        try
          let* env = interpret_block b env in
          interpret_stmt (Siterator (nl, [ (loc, Evalue cl) ], b)) env
        with Break_catch env -> Ok env
      in
      begin match ctrl_value with
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
      | _ -> assert false (* typing error *)
      end
    | iterator_func :: state :: ctrl_var :: _ ->
      (* Stateless iterator *)
      begin match ctrl_var with
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
    | _ -> assert false
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
