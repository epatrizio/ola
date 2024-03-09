(* Scope analysis *)

open Ast

let rec analyse_expr expr env =
  match expr with
  | loc, Evalue v -> ((loc, Evalue v), env)
  | loc, Eunop (op, e) ->
    let e, env = analyse_expr e env in
    ((loc, Eunop (op, e)), env)
  | loc, Ebinop (e1, op, e2) ->
    let e1, env = analyse_expr e1 env in
    let e2, env = analyse_expr e2 env in
    ((loc, Ebinop (e1, op, e2)), env)
  | loc, Evariadic -> ((loc, Evariadic), env)
  | loc, Efunctiondef (pl, b) ->
    let (pl, b), env = analyse_funcbody (pl, b) env in
    ((loc, Efunctiondef (pl, b)), env)
  | loc, Eprefix pexp ->
    let pexp, env = analyse_prefixexp pexp env in
    ((loc, Eprefix pexp), env)
  | loc, Etableconstructor fl ->
    let fl, env = analyse_fieldlist fl env in
    ((loc, Etableconstructor fl), env)

and analyse_el el env =
  List.fold_left
    (fun (expl, ev) exp ->
      let exp, ev = analyse_expr exp ev in
      (expl @ [ exp ], ev) )
    ([], env) el

and analyse_var var env =
  match var with
  | VarName n ->
    let fresh_n, env = Env.get_name n (Vnil ()) env in
    (VarName fresh_n, env)
  | VarTableField (pexp, exp) ->
    let pexp, env = analyse_prefixexp pexp env in
    let exp, env = analyse_expr exp env in
    (VarTableField (pexp, exp), env)

and analyse_fieldlist fl env =
  match fl with
  | [] -> ([], env)
  | f :: fl -> (
    match f with
    | Fexp e ->
      let e, env = analyse_expr e env in
      let fl, env = analyse_fieldlist fl env in
      (Fexp e :: fl, env)
    | Fname (n, e) ->
      (* n should not be freshed: tbl.x > tbl["x"] *)
      let e, env = analyse_expr e env in
      let fl, env = analyse_fieldlist fl env in
      (Fname (n, e) :: fl, env)
    | Fcol (e1, e2) ->
      let e1, env = analyse_expr e1 env in
      let e2, env = analyse_expr e2 env in
      let fl, env = analyse_fieldlist fl env in
      (Fcol (e1, e2) :: fl, env) )

and analyse_namelist nl env =
  List.fold_left
    (fun (nl, ev) n ->
      let n, ev = Env.add_local n (Vnil ()) ev in
      (nl @ [ n ], ev) )
    ([], env) nl

and analyse_parlist pl env =
  match pl with
  | PLlist (nl, eo) ->
    let nl, env = analyse_namelist nl env in
    let eo, env =
      match eo with
      | None -> (None, env)
      | Some e ->
        let e, env = analyse_expr e env in
        (Some e, env)
    in
    (PLlist (nl, eo), env)
  | PLvariadic e ->
    let e, env = analyse_expr e env in
    (PLvariadic e, env)

and analyse_funcbody ((pl, b) as _fb) env =
  let pl, env_loc = analyse_parlist pl env in
  let b, env_loc = analyse_block b env_loc in
  let locals = Env.get_locals env in
  let env = Env.with_locals env_loc locals in
  ((pl, b), env)

and analyse_args args env =
  match args with
  | Aexpl el ->
    let el, env = analyse_el el env in
    (Aexpl el, env)
  | Atable fl ->
    let fl, env = analyse_fieldlist fl env in
    (Atable fl, env)

and analyse_prefixexp pexp env =
  match pexp with
  | PEvar v ->
    let v, env = analyse_var v env in
    (PEvar v, env)
  | PEfunctioncall fc ->
    let fc, env = analyse_functioncall fc env in
    (PEfunctioncall fc, env)
  | PEexp e ->
    let e, env = analyse_expr e env in
    (PEexp e, env)

and analyse_functioncall fc env =
  match fc with
  | FCpreargs (pexp, args) ->
    let pexp, env = analyse_prefixexp pexp env in
    let args, env = analyse_args args env in
    (FCpreargs (pexp, args), env)
  | FCprename (pexp, n, args) ->
    let pexp, env = analyse_prefixexp pexp env in
    let args, env = analyse_args args env in
    (FCprename (pexp, n, args), env)

and analyse_stmt stmt env =
  let analyse_vl vl env =
    List.fold_left
      (fun (varl, ev) v ->
        let v, ev = analyse_var v ev in
        (varl @ [ v ], ev) )
      ([], env) vl
  in
  let analyse_nal nal env =
    (* todo : local name attrib (const/close) support *)
    List.fold_left
      (fun (nl, ev) (n, oa) ->
        let fresh_n, ev = Env.add_local n (Vnil ()) ev in
        (nl @ [ (fresh_n, oa) ], ev) )
      ([], env) nal
  in
  let analyse_ebl ebl env =
    List.fold_left
      (fun (exbl, ev) (e, b) ->
        let e, ev = analyse_expr e ev in
        let b, ev = analyse_block b ev in
        (exbl @ [ (e, b) ], ev) )
      ([], env) ebl
  in
  match stmt with
  | Sempty -> (Sempty, env)
  | Sassign (vl, el) ->
    let el, env = analyse_el el env in
    let vl, env = analyse_vl vl env in
    (Sassign (vl, el), env)
  | SassignLocal (nal, el) ->
    let el, env = analyse_el el env in
    let nal, env = analyse_nal nal env in
    (SassignLocal (nal, el), env)
  | Sbreak -> (Sbreak, env)
  | Sreturn el ->
    let el, env = analyse_el el env in
    (Sreturn el, env)
  | Slabel n -> (Slabel n, env)
  | Sgoto n -> (Sgoto n, env)
  | Sblock b ->
    let b, env = analyse_block b env in
    (Sblock b, env)
  | Swhile (e, b) ->
    let e, env = analyse_expr e env in
    let b, env = analyse_block b env in
    (Swhile (e, b), env)
  | Srepeat (b, e) ->
    let b, env = analyse_block b env in
    let e, env = analyse_expr e env in
    (Srepeat (b, e), env)
  | Sif (e, b, ebl, ob) ->
    let e, env = analyse_expr e env in
    let b, env = analyse_block b env in
    let ebl, env = analyse_ebl ebl env in
    let ob, env =
      match ob with
      | None -> (None, env)
      | Some b ->
        let b, env = analyse_block b env in
        (Some b, env)
    in
    (Sif (e, b, ebl, ob), env)
  | Sfor (n, e1, e2, oe, b) ->
    let fresh_n, env_loc = Env.add_local n (Vnil ()) env in
    let e1, env_loc = analyse_expr e1 env_loc in
    let e2, env_loc = analyse_expr e2 env_loc in
    let oe, env_loc =
      match oe with
      | None -> (None, env_loc)
      | Some e ->
        let e, env_loc = analyse_expr e env_loc in
        (Some e, env_loc)
    in
    let b, env_loc = analyse_block b env_loc in
    let locals = Env.get_locals env in
    let env = Env.with_locals env_loc locals in
    (Sfor (fresh_n, e1, e2, oe, b), env)
  | Siterator (nl, el, b) ->
    let fresh_nl, env_loc =
      List.fold_left
        (fun (nl, e) n ->
          let fresh_n, env_loc = Env.add_local n (Vnil ()) e in
          (nl @ [ fresh_n ], env_loc) )
        ([], env) nl
    in
    let analysed_el, env_loc =
      List.fold_left
        (fun (el, e) exp ->
          let analysed_e, env_loc = analyse_expr exp e in
          (el @ [ analysed_e ], env_loc) )
        ([], env_loc) el
    in
    let b, env_loc = analyse_block b env_loc in
    let locals = Env.get_locals env in
    let env = Env.with_locals env_loc locals in
    (Siterator (fresh_nl, analysed_el, b), env)
  (* | Sfunction (n, fb) ->
     let fresh_n, env = Env.get_funcname n env in
     let fb, env = analyse_funcbody fb env in
     (Sfunction (fresh_n, fb), env) *)
  (* | SfunctionLocal (n, fb) ->
     let fresh_n, env = Env.add_local_funcname n env in
     let fb, env = analyse_funcbody fb env in
     (SfunctionLocal (fresh_n, fb), env) *)
  | SfunctionCall fc ->
    let fc, env = analyse_functioncall fc env in
    (SfunctionCall fc, env)

and analyse_block b env =
  match b with
  | [] -> ([], env)
  | stmt :: tl ->
    let stmt, env_s = analyse_stmt stmt env in
    let tl, env_b = analyse_block tl env_s in

    let locals = Env.get_locals env in
    let env = Env.with_locals env_b locals in

    (stmt :: tl, env)

let analysis chunk env = analyse_block chunk env
