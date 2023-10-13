(* Scope analysis *)

open Ast

let rec analyse_expr expr env =
  match expr with
  | loc, Evalue v -> ((loc, Evalue v), env)
  | loc, Evar (Name n) ->
    let fresh_n, env = Env.add_name n env in
    ((loc, Evar (Name fresh_n)), env)
  | loc, Eunop (op, e) ->
    let e, env = analyse_expr e env in
    ((loc, Eunop (op, e)), env)
  | loc, Ebinop (op, e1, e2) ->
    let e1, env = analyse_expr e1 env in
    let e2, env = analyse_expr e2 env in
    ((loc, Ebinop (op, e1, e2)), env)

let rec analyse_stmt stmt env =
  let analyse_var var env =
    match var with
    | Name n ->
      let fresh_n, env = Env.add_name n env in
      (Name fresh_n, env)
  in
  let rec analyse_vl vl env =
    match vl with
    | [] -> ([], env)
    | v :: tl ->
      let v, env = analyse_var v env in
      let tl, env = analyse_vl tl env in
      (v :: tl, env)
  in
  let rec analyse_el el env =
    match el with
    | [] -> ([], env)
    | e :: tl ->
      let e, env = analyse_expr e env in
      let tl, env = analyse_el tl env in
      (e :: tl, env)
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
    let vl, env = analyse_vl vl env in
    let el, env = analyse_el el env in
    (Sassign (vl, el), env)
  | SassignLocal (nal, elo) ->
    (SassignLocal (nal, elo), env (* todo: to be implemented *))
  | Sbreak -> (Sbreak, env)
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
    (* todo: local environment for n *)
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
    (Sfor (n, e1, e2, oe, b), env)
  | Siterator (nl, el, b) ->
    (* todo: to be implemented *)
    (Siterator (nl, el, b), env)
  | Sprint e ->
    let e, env = analyse_expr e env in
    (Sprint e, env)

and analyse_block b env =
  match b with
  | [] -> ([], env)
  | stmt :: tl ->
    let stmt, env = analyse_stmt stmt env in
    let tl, env = analyse_block tl env in
    (stmt :: tl, env)

let analysis chunk env = analyse_block chunk env
