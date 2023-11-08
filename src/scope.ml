(* Scope analysis *)

open Ast

let rec analyse_expr expr env =
  match expr with
  | loc, Evalue v -> ((loc, Evalue v), env)
  | loc, Eunop (op, e) ->
    let e, env = analyse_expr e env in
    ((loc, Eunop (op, e)), env)
  | loc, Ebinop (op, e1, e2) ->
    let e1, env = analyse_expr e1 env in
    let e2, env = analyse_expr e2 env in
    ((loc, Ebinop (op, e1, e2)), env)
  | loc, Evariadic -> ((loc, Evariadic), env)
  | loc, Efunctiondef (pl, b) ->
    let (pl, b), env = analyse_funcbody (pl, b) env in
    ((loc, Efunctiondef (pl, b)), env)
  | loc, Eprefix (PEvar (VarName n)) ->
    let fresh_n, env = Env.get_name n env in
    ((loc, Eprefix (PEvar (VarName fresh_n))), env)
  | loc, Eprefix (PEvar v) ->
    (* TODO for table *)
    ((loc, Eprefix (PEvar v)), env)
  | loc, Eprefix (PEexp e) ->
    let e, env = analyse_expr e env in
    ((loc, Eprefix (PEexp e)), env)
  | loc, Eprefix (PEfunctioncall fc) ->
    let fc, env = analyse_functioncall fc env in
    ((loc, Eprefix (PEfunctioncall fc)), env)
  | loc, Etableconstructor flo -> ((loc, Etableconstructor flo), env)
(* TODO *)

and analyse_el el env =
  (* List.fold_left
     (fun (expl, ev) exp -> let exp, ev = analyse_expr exp ev in (exp :: expl, ev))
     ([], env) el *)
  (* todo: bug in fold version *)
  match el with
  | [] -> ([], env)
  | e :: tl ->
    let e, env = analyse_expr e env in
    let tl, env = analyse_el tl env in
    (e :: tl, env)

and analyse_namelist nl env =
  (* List.fold_left
     (fun (nl, ev) n -> let n, ev = Env.add_local n ev in (n :: nl, ev))
     ([], env) nl *)
  match nl with
  | [] -> ([], env)
  | n :: tl ->
    let n, env = Env.add_local n env in
    let tl, env = analyse_namelist tl env in
    (n :: tl, env)

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

and analyse_funcbody fb env =
  let pl, b = fb in
  let pl, env_loc = analyse_parlist pl env in
  let b, env_loc = analyse_block b env_loc in
  let locals = Env.get_locals env in
  let env = Env.with_locals env_loc locals in
  ((pl, b), env)

and analyse_args args env =
  match args with
  | Aexpl el -> let el, env = analyse_el el env in (Aexpl el, env)
  | Atable flo -> (Atable flo, env) (* todo *)
  | Astr s -> (Astr s, env) (* todo *)

(* todo - only string VarName
    | VarTableField of prefixexp * expr
    | VarTableFieldName of prefixexp * string   
*)
and analyse_var is_local var env =
  match var with
  | n ->
    let fresh_n, env =
      (if is_local then Env.add_local else Env.get_name) n env
    in
    (fresh_n, env)

and analyse_prefixexp pexp env =
  match pexp with
  | PEvar VarName n -> let n, env = analyse_var false n env in (PEvar (VarName n), env)
  | PEvar v -> (PEvar v, env)
  | PEfunctioncall fc -> let fc, env = analyse_functioncall fc env in (PEfunctioncall fc, env)
  | PEexp e -> let e, env = analyse_expr e env in (PEexp e, env)

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
  (* todo - only string VarName *)
  (* let analyse_var is_local var env =
    match var with
    | n ->
      let fresh_n, env =
        (if is_local then Env.add_local else Env.get_name) n env
      in
      (fresh_n, env)
  in *)
  let rec analyse_vl vl env =
    match vl with
    | [] -> ([], env)
    | v :: tl -> (
      match v with
      | VarName n ->
        let n, env = analyse_var false n env in
        let tl, env = analyse_vl tl env in
        (VarName n :: tl, env)
      | VarTableField (_pexp, _expr) -> (v :: tl, env)
      | VarTableFieldName (_pexp, _n) -> (v :: tl, env) )
  in
  let rec analyse_nal nal env =
    (* todo : local name attrib (const/close) support *)
    match nal with
    | [] -> ([], env)
    | (n, on) :: tl ->
      let n, env = analyse_var true n env in
      let tl, env = analyse_nal tl env in
      ((n, on) :: tl, env)
  in
  let analyse_elo elo env =
    match elo with
    | None -> (None, env)
    | Some el ->
      let el, env = analyse_el el env in
      (Some el, env)
  in
  let rec analyse_ebl ebl env =
    match ebl with
    | [] -> ([], env)
    | (e, b) :: tl ->
      let e, env = analyse_expr e env in
      let b, env = analyse_block b env in
      let tl, env = analyse_ebl tl env in
      ((e, b) :: tl, env)
  in
  match stmt with
  | Sempty -> (Sempty, env)
  | Sassign (vl, el) ->
    let el, env = analyse_el el env in
    let vl, env = analyse_vl vl env in
    (Sassign (vl, el), env)
  | SassignLocal (nal, elo) ->
    let elo, env = analyse_elo elo env in
    let nal, env = analyse_nal nal env in
    (SassignLocal (nal, elo), env)
  | Sbreak -> (Sbreak, env)
  | Sreturn (el, so) ->
    let el, env = analyse_el el env in
    let so, env =
      match so with
      | None -> (None, env)
      | Some s ->
        let s, env = analyse_stmt s env in
        (Some s, env)
    in
    (Sreturn (el, so), env)
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
    let fresh_n, env_loc = Env.add_local n env in
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
    (* todo: to be implemented *)
    (Siterator (nl, el, b), env)
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
  | Sprint e ->
    let e, env = analyse_expr e env in
    (Sprint e, env)

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
