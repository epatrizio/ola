open Ast

let () = Random.self_init ()

type block_pointer =
  | Begin
  | Last
  | Label of string

exception Goto_catch of block_pointer * Env.t

exception Break_catch of Env.t

exception Return_catch of expr list * stmt option * Env.t

exception Interpretation_error of location option * string

let error loc message = raise (Interpretation_error (loc, message))

let typecheck_expr expr env =
  match Typer.typecheck_expr expr env with
  | Ok _t -> ()
  | Error (loc, msg) -> error (Some loc) (Format.sprintf "Typing error: %s" msg)

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

let rec name_of_prefixexp pexp env =
  match pexp with
  | PEvar (VarName n) -> n
  | PEvar (VarTableField (pexp, _exp)) -> name_of_prefixexp pexp env
  | PEvar (VarTableFieldName (pexp, _s)) -> name_of_prefixexp pexp env
  | PEfunctioncall (FCpreargs (pexp, _a)) -> name_of_prefixexp pexp env
  | PEfunctioncall (FCprename (pexp, _s, _a)) -> name_of_prefixexp pexp env
  | PEexp _exp -> assert false (* todo *)

let rec interpret_bbinop_expr binop ((loc1, _) as expr1) ((loc2, _) as expr2)
  env =
  (* todo: ok for num values *)
  let v1, env = interpret_expr expr1 env in
  let v2, env = interpret_expr expr2 env in
  typecheck_expr
    (loc1, Ebinop (binop, (loc1, Evalue v1), (loc2, Evalue v2)))
    env;
  match (v1, v2) with
  | Vboolean b1, Vboolean b2 -> begin
    match binop with
    | Band -> (Vboolean (b1 && b2), env)
    | Bor -> (Vboolean (b1 || b2), env)
    | _ -> assert false (* call error *)
  end
  | _ -> assert false (* typing error *)

and interpret_ibinop_expr binop ((loc1, _) as expr1) ((loc2, _) as expr2) env =
  let v1, env = interpret_expr expr1 env in
  let v2, env = interpret_expr expr2 env in
  typecheck_expr
    (loc1, Ebinop (binop, (loc1, Evalue v1), (loc2, Evalue v2)))
    env;
  match (v1, v2) with
  | Vnumber (Ninteger i1), Vnumber (Ninteger i2) -> begin
    match binop with
    | Badd -> (Vnumber (Ninteger (i1 + i2)), env)
    | Bsub -> (Vnumber (Ninteger (i1 - i2)), env)
    | Bmul -> (Vnumber (Ninteger (i1 * i2)), env)
    | Bdiv -> (Vnumber (Nfloat (float_of_int i1 /. float_of_int i2)), env)
    | Bfldiv -> (Vnumber (Ninteger (i1 / i2)), env (* todo div by 0 error *))
    | Bmod -> (Vnumber (Ninteger (i1 mod i2)), env)
    | Bexp ->
      (Vnumber (Nfloat (Float.pow (float_of_int i1) (float_of_int i2))), env)
    | Bland -> (Vnumber (Ninteger (i1 land i2)), env)
    | Blor -> (Vnumber (Ninteger (i1 lor i2)), env)
    | Blxor -> (Vnumber (Ninteger (i1 lxor i2)), env)
    | Blsl -> (Vnumber (Ninteger (i1 lsl i2)), env)
    | Blsr -> (Vnumber (Ninteger (i1 lsr i2)), env)
    | Blt -> (Vboolean (i1 < i2), env)
    | Ble -> (Vboolean (i1 <= i2), env)
    | Bgt -> (Vboolean (i1 > i2), env)
    | Bge -> (Vboolean (i1 >= i2), env)
    | Beq -> (Vboolean (i1 = i2), env)
    | Bneq -> (Vboolean (i1 != i2), env)
    | _ -> assert false (* call error *)
  end
  | Vnumber (Nfloat f), Vnumber (Ninteger i) -> begin
    match binop with
    | Badd -> (Vnumber (Nfloat (f +. float_of_int i)), env)
    | Bsub -> (Vnumber (Nfloat (f -. float_of_int i)), env)
    | Bmul -> (Vnumber (Nfloat (f *. float_of_int i)), env)
    | Bdiv -> (Vnumber (Nfloat (f /. float_of_int i)), env)
    | Bfldiv ->
      let _, q = Float.modf (f /. float_of_int i) in
      (Vnumber (Nfloat q), env)
    | Bmod ->
      let fi = float_of_int i in
      let _, q = Float.modf (f /. fi) in
      (Vnumber (Nfloat (f -. (fi *. q))), env)
    | Bexp -> (Vnumber (Nfloat (Float.pow f (float_of_int i))), env)
    | Bland -> (Vnumber (Ninteger (get_int f loc1 land i)), env)
    | Blor -> (Vnumber (Ninteger (get_int f loc1 lor i)), env)
    | Blxor -> (Vnumber (Ninteger (get_int f loc1 lxor i)), env)
    | Blsl -> (Vnumber (Ninteger (get_int f loc1 lsl i)), env)
    | Blsr -> (Vnumber (Ninteger (get_int f loc1 lsr i)), env)
    | Blt -> (Vboolean (f < float_of_int i), env)
    | Ble -> (Vboolean (f <= float_of_int i), env)
    | Bgt -> (Vboolean (f > float_of_int i), env)
    | Bge -> (Vboolean (f >= float_of_int i), env)
    | Beq -> (Vboolean (f = float_of_int i), env)
    | Bneq -> (Vboolean (f != float_of_int i), env)
    | _ -> assert false (* call error *)
  end
  | Vnumber (Ninteger i), Vnumber (Nfloat f) -> begin
    match binop with
    | Badd -> (Vnumber (Nfloat (float_of_int i +. f)), env)
    | Bsub -> (Vnumber (Nfloat (float_of_int i -. f)), env)
    | Bmul -> (Vnumber (Nfloat (float_of_int i *. f)), env)
    | Bdiv -> (Vnumber (Nfloat (float_of_int i /. f)), env)
    | Bfldiv ->
      let _, q = Float.modf (float_of_int i /. f) in
      (Vnumber (Nfloat q), env)
    | Bmod ->
      let fi = float_of_int i in
      let _, q = Float.modf (fi /. f) in
      (Vnumber (Nfloat (fi -. (f *. q))), env)
    | Bexp -> (Vnumber (Nfloat (Float.pow (float_of_int i) f)), env)
    | Bland -> (Vnumber (Ninteger (i land get_int f loc2)), env)
    | Blor -> (Vnumber (Ninteger (i lor get_int f loc2)), env)
    | Blxor -> (Vnumber (Ninteger (i lxor get_int f loc2)), env)
    | Blsl -> (Vnumber (Ninteger (i lsl get_int f loc2)), env)
    | Blsr -> (Vnumber (Ninteger (i lsr get_int f loc2)), env)
    | Blt -> (Vboolean (float_of_int i < f), env)
    | Ble -> (Vboolean (float_of_int i <= f), env)
    | Bgt -> (Vboolean (float_of_int i > f), env)
    | Bge -> (Vboolean (float_of_int i >= f), env)
    | Beq -> (Vboolean (float_of_int i = f), env)
    | Bneq -> (Vboolean (float_of_int i != f), env)
    | _ -> assert false (* call error *)
  end
  | Vnumber (Nfloat f1), Vnumber (Nfloat f2) -> begin
    match binop with
    | Badd -> (Vnumber (Nfloat (f1 +. f2)), env)
    | Bsub -> (Vnumber (Nfloat (f1 -. f2)), env)
    | Bmul -> (Vnumber (Nfloat (f1 *. f2)), env)
    | Bdiv -> (Vnumber (Nfloat (f1 /. f2)), env)
    | Bfldiv ->
      let _, q = Float.modf (f1 /. f2) in
      (Vnumber (Nfloat q), env)
    | Bmod ->
      let _, q = Float.modf (f1 /. f2) in
      (Vnumber (Nfloat (f1 -. (f2 *. q))), env)
    | Bexp -> (Vnumber (Nfloat (Float.pow f1 f2)), env)
    | Bland -> (Vnumber (Ninteger (get_int f1 loc1 land get_int f2 loc2)), env)
    | Blor -> (Vnumber (Ninteger (get_int f1 loc1 lor get_int f2 loc2)), env)
    | Blxor -> (Vnumber (Ninteger (get_int f1 loc1 lxor get_int f2 loc2)), env)
    | Blsl -> (Vnumber (Ninteger (get_int f1 loc1 lsl get_int f2 loc2)), env)
    | Blsr -> (Vnumber (Ninteger (get_int f1 loc1 lsr get_int f2 loc2)), env)
    | Blt -> (Vboolean (f1 < f2), env)
    | Ble -> (Vboolean (f1 <= f2), env)
    | Bgt -> (Vboolean (f1 > f2), env)
    | Bge -> (Vboolean (f1 >= f2), env)
    | Beq -> (Vboolean (f1 = f2), env)
    | Bneq -> (Vboolean (f1 != f2), env)
    | _ -> assert false (* call error *)
  end
  | _ -> assert false (* typing error *)

and interpret_sbinop_expr ((loc1, _) as expr1) ((loc2, _) as expr2) env =
  let v1, env = interpret_expr expr1 env in
  let v2, env = interpret_expr expr2 env in
  typecheck_expr
    (loc1, Ebinop (Bddot, (loc1, Evalue v1), (loc2, Evalue v2)))
    env;
  begin
    match (v1, v2) with
    | Vstring s1, Vstring s2 -> (Vstring (s1 ^ s2), env)
    | Vstring s, Vnumber (Ninteger i) -> (Vstring (s ^ string_of_int i), env)
    | Vstring s, Vnumber (Nfloat f) -> (Vstring (s ^ string_of_float f), env)
    | Vnumber (Ninteger i), Vstring s -> (Vstring (string_of_int i ^ s), env)
    | Vnumber (Nfloat f), Vstring s -> (Vstring (string_of_float f ^ s), env)
    | Vnumber (Ninteger i1), Vnumber (Ninteger i2) ->
      (Vstring (string_of_int i1 ^ string_of_int i2), env)
    | Vnumber (Nfloat f1), Vnumber (Nfloat f2) ->
      (Vstring (string_of_float f1 ^ string_of_float f2), env)
    | Vnumber (Ninteger i), Vnumber (Nfloat f) ->
      (Vstring (string_of_int i ^ string_of_float f), env)
    | Vnumber (Nfloat f), Vnumber (Ninteger i) ->
      (Vstring (string_of_float f ^ string_of_int i), env)
    | _ -> assert false (* typing error *)
  end

and interpret_prefixexp pexp env =
  match pexp with
  | PEvar v -> interpret_var v env
  | PEexp exp -> interpret_expr exp env
  | PEfunctioncall fc ->
    let v, _env = interpret_functioncall fc env in
    (v, env)

and interpret_var v env =
  match v with
  | VarName n -> (Env.get_value n env, env)
  | VarTableField (pexp, exp) ->
    let t, env = interpret_prefixexp pexp env in
    let idx, env = interpret_expr exp env in
    begin
      match t with
      | Vtable (_i, tbl) -> begin
        match Table.get idx tbl with
        | None -> (Vnil (), env)
        | Some v -> (v, env)
      end
      | _ -> assert false (* typing error *)
    end
  | VarTableFieldName (_pexp, _s) -> (Vnil (), env)
(* todo *)

and interpret_field field env =
  match field with
  | Fexp exp -> interpret_expr exp env
  | Fname (_s, _exp) -> (Vnil (), env) (* todo *)
  | Fcol (_exp1, _exp2) -> (Vnil (), env)
(* todo *)

and interpret_expr (loc, expr) env =
  match expr with
  | Evalue
      ( ( Vnil ()
        | Vboolean _ | Vnumber _ | Vstring _ | Vfunction _ | VfunctionReturn _
        | Vtable _ ) as v ) ->
    (v, env)
  | Eunop (Unot, ((l, _) as e)) ->
    let v, env = interpret_expr e env in
    typecheck_expr (loc, Eunop (Unot, (l, Evalue v))) env;
    begin
      match v with
      | Vboolean b -> (Vboolean (not b), env)
      | _ -> assert false (* typing error *)
    end
  | Eunop (Uminus, ((l, _) as e)) ->
    let v, env = interpret_expr e env in
    typecheck_expr (loc, Eunop (Uminus, (l, Evalue v))) env;
    begin
      match v with
      | Vnumber (Ninteger i) -> (Vnumber (Ninteger (-i)), env)
      | Vnumber (Nfloat f) -> (Vnumber (Nfloat (-.f)), env)
      | _ -> assert false (* typing error *)
    end
  | Eunop (Usharp, ((l, _) as e)) ->
    let v, env = interpret_expr e env in
    typecheck_expr (loc, Eunop (Usharp, (l, Evalue v))) env;
    begin
      match v with
      | Vstring s -> (Vnumber (Ninteger (String.length s)), env)
      | _ -> assert false (* typing error *)
    end
  | Eunop (Ulnot, ((l, _) as e)) ->
    let v, env = interpret_expr e env in
    typecheck_expr (loc, Eunop (Ulnot, (l, Evalue v))) env;
    begin
      match v with
      | Vnumber (Ninteger i) -> (Vnumber (Ninteger (lnot i)), env)
      | Vnumber (Nfloat f) -> (Vnumber (Ninteger (lnot (get_int f loc))), env)
      | _ -> assert false (* typing error *)
    end
  | Ebinop (((Band | Bor) as op), e1, e2) -> interpret_bbinop_expr op e1 e2 env
  | Ebinop (Bddot, e1, e2) -> interpret_sbinop_expr e1 e2 env
  | Ebinop (op, e1, e2) -> interpret_ibinop_expr op e1 e2 env
  | Evariadic -> (Vnil (), env)
  | Efunctiondef fb -> (Vfunction (Random.bits32 (), fb), env)
  | Eprefix pexp -> interpret_prefixexp pexp env
  | Etableconstructor fl ->
    let table, _i, env =
      List.fold_left
        (fun (tbl, idx, ev) f ->
          let v, ev = interpret_field f ev in
          (Table.add (Vnumber (Ninteger idx)) v tbl, idx + 1, ev) )
        (Table.empty, 1, env) fl
    in
    (Vtable (Random.bits32 (), table), env)

and set_var v value env =
  match v with
  | VarName n -> Env.set_value n value env
  | VarTableField (pexp, exp) ->
    let t, env = interpret_prefixexp pexp env in
    let idx, env = interpret_expr exp env in
    begin
      match t with
      | Vtable (i, tbl) ->
        let tbl = Table.add idx value tbl in
        let n = name_of_prefixexp pexp env in
        Env.set_value n (Vtable (i, tbl)) env
      | _ -> assert false (* typing error *)
    end
  | VarTableFieldName _ -> env (* todo *)

and lists_assign vl el env =
  begin
    match (vl, el) with
    | [], [] | [], _ -> env
    | vl, [] -> List.fold_left (fun e v -> set_var v (Vnil ()) e) env vl
    | v :: vl, [ ((l, _e) as e) ] -> (
      let va, env = interpret_expr e env in
      match va with
      | VfunctionReturn vall -> begin
        match vall with
        | [] -> set_var v (Vnil ()) env
        | va :: vall ->
          let env = set_var v va env in
          let vall_exp = List.map (fun v -> (l, Evalue v)) vall in
          lists_assign vl vall_exp env
      end
      | va ->
        let env = set_var v va env in
        lists_assign vl [] env )
    | v :: vl, e :: el -> (
      let va, env = interpret_expr e env in
      match va with
      | VfunctionReturn vall -> begin
        match vall with
        | [] -> set_var v (Vnil ()) env
        | va :: _vall ->
          let env = set_var v va env in
          lists_assign vl el env
      end
      | va ->
        let env = set_var v va env in
        lists_assign vl el env )
  end

and lists_lassign nal el env =
  begin
    match (nal, el) with
    | [], [] | [], _ -> env
    | nal, [] ->
      List.fold_left (fun e (n, _on) -> Env.set_value n (Ast.Vnil ()) e) env nal
    | (n, _on) :: vl, [ ((l, _e) as e) ] -> (
      let va, env = interpret_expr e env in
      match va with
      | VfunctionReturn vall -> begin
        match vall with
        | [] -> Env.set_value n (Ast.Vnil ()) env
        | va :: vall ->
          let env = Env.set_value n va env in
          let vall_exp = List.map (fun v -> (l, Evalue v)) vall in
          lists_lassign vl vall_exp env
      end
      | va ->
        let env = Env.set_value n va env in
        lists_lassign vl [] env )
    | (n, _on) :: vl, e :: el -> (
      let va, env = interpret_expr e env in
      match va with
      | VfunctionReturn vall -> begin
        match vall with
        | [] -> Env.set_value n (Ast.Vnil ()) env
        | va :: _vall ->
          let env = Env.set_value n va env in
          lists_lassign vl el env
      end
      | va ->
        let env = Env.set_value n va env in
        lists_lassign vl el env )
  end

and lists_args pl el env =
  match pl with
  | PLvariadic _ -> assert false (* to be implemented *)
  | PLlist (_nl, Some _) -> assert false (* to be implemented *)
  | PLlist (nl, None) ->
    let vl = List.map (fun n -> VarName n) nl in
    lists_assign vl el env

and interpret_fct value el env =
  match value with
  | Vfunction (_i, (pl, b)) -> begin
    try
      let env = lists_args pl el env in
      let env = interpret_block b env in
      (VfunctionReturn [], env)
    with Return_catch (el, _so, env) -> (
      match el with
      | [] -> (VfunctionReturn [], env)
      | [ e ] -> interpret_expr e env
      | el ->
        let vl, env =
          List.fold_left
            (fun (vl, e) exp ->
              let v, e = interpret_expr exp e in
              (vl @ [ v ], e) )
            ([], env) el
        in
        (VfunctionReturn vl, env) )
  end
  | _ -> error (*Some loc*) None "function call error!"

and interpret_functioncall fc env =
  match fc with
  (* | FCpreargs (PEexp ((loc, _) as e), Aexpl el) -> *)
  | FCpreargs (PEvar (VarName v), Aexpl el) ->
    (* let e, env = interpret_expr e env in *)
    let value = Env.get_value v env in
    interpret_fct value el env
  | FCpreargs (PEvar (VarTableField (pexp, exp)), Aexpl el) ->
    let t, env = interpret_prefixexp pexp env in
    let idx, env = interpret_expr exp env in
    begin
      match t with
      | Vtable (_i, tbl) -> begin
        match Table.get idx tbl with
        | None -> assert false
        | Some v -> interpret_fct v el env
      end
      | _ -> assert false (* typing error *)
    end
  | _ -> assert false

and interpret_stmt stmt env =
  match stmt with
  | Sempty -> env
  | Sassign (vl, el) -> lists_assign vl el env
  | SassignLocal (nal, el) -> lists_lassign nal el env
  | Sbreak -> raise (Break_catch env)
  | Sreturn (el, so) -> raise (Return_catch (el, so, env))
  | Slabel _ -> env
  | Sgoto n -> raise (Goto_catch (Label n, env))
  | Sblock b -> interpret_block b env
  | Swhile (e, b) ->
    (* Doc: The condition expression of a control structure can return any value.
       Both false and nil test false. *)
    let cond, env = interpret_expr e env in
    begin
      match cond with
      | Vboolean false | Vnil () -> env
      | _ -> (
        try
          let env = interpret_block b env in
          interpret_stmt (Swhile (e, b)) env
        with Break_catch env -> env )
    end
  | Srepeat (b, e) -> begin
    try
      let env = interpret_block b env in
      let cond, env = interpret_expr e env in
      match cond with
      | Vboolean false | Vnil () -> interpret_stmt (Srepeat (b, e)) env
      | _ -> env
    with Break_catch env -> env
  end
  | Sif (e, b, ebl, ob) ->
    let rec interpret_elseif ebl env =
      match ebl with
      | [] -> (None, env)
      | (e, b) :: tl ->
        let cond, env = interpret_expr e env in
        begin
          match cond with
          | Vboolean false | Vnil () -> interpret_elseif tl env
          | _ ->
            let env = interpret_block b env in
            (Some (), env)
        end
    in
    let cond, env = interpret_expr e env in
    begin
      match cond with
      | Vboolean false | Vnil () ->
        let opt, env = interpret_elseif ebl env in
        begin
          match opt with
          | Some () -> env
          | None -> begin
            match ob with Some b -> interpret_block b env | None -> env
          end
        end
      | _ -> interpret_block b env
    end
  | Sfor (n, e1, e2, oe, b) ->
    let init_val expr env =
      let v, env = interpret_expr expr env in
      match v with
      | Vnumber (Ninteger i) -> (Vnumber (Ninteger i), env)
      | Vnumber (Nfloat f) -> (Vnumber (Nfloat f), env)
      | Vstring s -> begin
        match float_of_string_opt s with
        | Some f -> (Vnumber (Nfloat f), env)
        | None -> assert false (* typing error *)
      end
      | _ -> assert false (* typing error *)
    in
    let cond_expr loc limit step _env =
      let op =
        match step with
        | Vnumber (Ninteger i) -> if i >= 0 then Ble else Bge
        | Vnumber (Nfloat f) -> if f >= 0. then Ble else Bge
        | _ -> assert false (* call error *)
      in
      (loc, Ebinop (op, (loc, Eprefix (PEvar (VarName n))), (loc, Evalue limit)))
    in
    let incr_cnt_stmt loc step =
      Sassign
        ( [ VarName n ]
        , [ ( loc
            , Ebinop
                (Badd, (loc, Eprefix (PEvar (VarName n))), (loc, Evalue step))
            )
          ] )
    in
    let l1, _e1 = e1 in
    let ival, env = init_val e1 env in
    let env = Env.set_value n ival env in
    let limit, env = init_val e2 env in
    let step, env =
      match oe with
      | Some e -> init_val e env
      | None -> (Vnumber (Ninteger 1), env)
    in
    (* Syntactic sugar *)
    interpret_stmt
      (Swhile (cond_expr l1 limit step env, b @ [ incr_cnt_stmt l1 step ]))
      env
  | Siterator (_nl, _el, _b) -> env (* todo: to be implemented *)
  (* | Sfunction (_n, _fb) -> env *)
  (* | SfunctionLocal (_n, _fb) -> env *)
  | SfunctionCall fc ->
    let _v, env = interpret_functioncall fc env in
    env
  | Sprint e -> (
    let v, _env = interpret_expr e env in
    match v with
    | Vstring s ->
      print_endline s;
      env
    | v ->
      Format.fprintf Format.std_formatter "%a@." print_value v;
      env )

and interpret_block b env =
  try List.fold_left (fun e stmt -> interpret_stmt stmt e) env b
  with Return_catch (el, so, env) -> raise (Return_catch (el, so, env))

let rec run ?(pt = Begin) chunk env =
  try
    let bl = block_from_pointer pt chunk in
    interpret_block bl env
  with
  | Goto_catch (label, env) -> run ~pt:label chunk env
  | Return_catch (_el, _so, env) -> env
