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
  | loc, Efunctiondef fb ->
    (* memo: name list scope *)
    ((loc, Efunctiondef fb), env)
  | loc, Eprefix (PEvar (Name n)) ->
    let fresh_n, env = Env.get_name n env in
    ((loc, Eprefix (PEvar (Name fresh_n))), env)
  | loc, Eprefix (PEexp e) ->
    let e, env = analyse_expr e env in
    ((loc, Eprefix (PEexp e)), env)
  | loc, Eprefix (PEfunctioncall fc) -> ((loc, Eprefix (PEfunctioncall fc)), env)
(* to be implemented *)

and analyse_stmt stmt env =
  let analyse_var is_local var env =
    match var with
    | Name n ->
      let fresh_n, env =
        (if is_local then Env.add_local else Env.get_name) n env
      in
      (Name fresh_n, env)
  in
  let rec analyse_vl vl env =
    match vl with
    | [] -> ([], env)
    | v :: tl ->
      let v, env = analyse_var false v env in
      let tl, env = analyse_vl tl env in
      (v :: tl, env)
  in
  let rec analyse_nal nal env =
    (* todo : local name attrib (const/close) support *)
    match nal with
    | [] -> ([], env)
    | (n, on) :: tl ->
      let Name n, env = analyse_var true (Name n) env in
      let tl, env = analyse_nal tl env in
      ((n, on) :: tl, env)
  in
  let rec analyse_el el env =
    match el with
    | [] -> ([], env)
    | e :: tl ->
      let e, env = analyse_expr e env in
      let tl, env = analyse_el tl env in
      (e :: tl, env)
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
  | Sreturn (elo, so) ->
    let elo, env = analyse_elo elo env in
    let so, env =
      match so with
      | None -> (None, env)
      | Some s ->
        let s, env = analyse_stmt s env in
        (Some s, env)
    in
    (Sreturn (elo, so), env)
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
    let fresh_n, env = Env.get_name n env in
    let e1, env = analyse_expr e1 env in
    let e2, env = analyse_expr e2 env in
    let oe, env =
      match oe with
      | None -> (None, env)
      | Some e ->
        let e, env = analyse_expr e env in
        (Some e, env)
    in
    let b, env = analyse_block b env in
    (Sfor (fresh_n, e1, e2, oe, b), env)
  | Siterator (nl, el, b) ->
    (* todo: to be implemented *)
    (Siterator (nl, el, b), env)
  | Sfunction (n, fb) -> (Sfunction (n, fb), env)
  | SfunctionLocal (n, fb) -> (SfunctionLocal (n, fb), env)
  | SfunctionCall fc -> (SfunctionCall fc, env)
  | Sprint e ->
    let e, env = analyse_expr e env in
    (Sprint e, env)

and analyse_block b env =
  match b with
  | [] -> ([], env)
  | stmt :: tl ->
    let stmt, env_s = analyse_stmt stmt env in
    let tl, env_b = analyse_block tl env_s in
    ( stmt :: tl
    , { names = env_b.names
      ; values = env_b.values
      ; globals = env_b.globals
      ; locals = env.locals
      } )

let analysis chunk env = analyse_block chunk env
