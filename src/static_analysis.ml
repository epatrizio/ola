(* Static analysis *)

open Ast
open Utils

exception Static_analysis_error of Ast.location option * string

let error loc_opt message = raise (Static_analysis_error (loc_opt, message))

(* 1. Variadic_func:
    Check if a function definition is variadic.
    If so, check if the use of Evariadic expression is permitted in the function body
  *)
module Variadic_func : sig
  val analyze : Ast.block -> (unit, 'a) result
end = struct
  let is_variadic_function parlist =
    match parlist with
    | PLvariadic | PLlist (_, true) -> true
    | PLlist (_, false) -> false

  let rec analyze_expr ((loc, _expr') as expr) =
    match snd expr with
    | Evalue _ -> false
    | Eunop (_, e) -> analyze_expr e
    | Ebinop (e1, _, e2) -> analyze_expr e1 || analyze_expr e2
    | Evariadic -> true
    | Efunctiondef (parlist, block) -> analyze_functiondef parlist block loc
    | Eprefix prefixexp -> analyze_prefixexp prefixexp
    | Etableconstructor fl -> check_list fl analyze_field

  and analyze_functiondef parlist block loc =
    if is_variadic_function parlist then false
    else
      let is_variadic_block = analyze_block block in
      if is_variadic_block then
        error (Some loc) "cannot use '...' outside a vararg function near '...'"
      else false

  and analyze_prefixexp prefixexp =
    match prefixexp with
    | PEvar _ | PEfunctioncall _ -> false
    | PEexp e -> analyze_expr e

  and analyze_field field =
    match field with
    | Fexp e | Fname (_, e) -> analyze_expr e
    | Fcol (e1, e2) -> analyze_expr e1 || analyze_expr e2

  and analyze_stmt stmt =
    match stmt with
    | Sassign (_, el) | SassignLocal (_, el) -> check_list el analyze_expr
    | Sempty | Sbreak | Sreturn _ | Slabel _ | Sgoto _ | SfunctionCall _ ->
      false
    | Sblock b | Swhile (_, b) | Srepeat (b, _) -> analyze_block b
    | Sif (e, b, ebl, ob) ->
      let be = analyze_expr e in
      let bb = analyze_block b in
      let bebl =
        check_list ebl (fun (e, b) ->
          let is_variadic_e = analyze_expr e in
          let is_variadic_b = analyze_block b in
          is_variadic_e || is_variadic_b )
      in
      let bob = match ob with None -> false | Some b -> analyze_block b in
      be || bb || bebl || bob
    | Sfor (_, e1, e2, oe, b) ->
      let be1 = analyze_expr e1 in
      let be2 = analyze_expr e2 in
      let boe = match oe with None -> false | Some e -> analyze_expr e in
      let bb = analyze_block b in
      be1 || be2 || boe || bb
    | Siterator (_, el, b) ->
      let bel = check_list el analyze_expr in
      let bb = analyze_block b in
      bel || bb

  and analyze_block block = check_list block analyze_stmt

  let analyze chunk =
    try
      let _ = analyze_block chunk in
      Ok ()
    with Static_analysis_error (loc, message) -> error loc message
end

(* 2. Const_var: Check assignment of <const> local variables *)
(* Nb. <close> attribute isn't support (other concept).
    https://www.lua.org/manual/5.4/manual.html#3.3.8 (To-be-closed Variables) *)
module Const_var : sig
  val analyze : Ast.block -> (unit, 'a) result
end = struct
  module SMap = Map.Make (String)

  let is_const_var attrib = String.equal attrib "const"

  let rec analyze_expr expr env =
    match snd expr with
    | Evalue _ -> env
    | Eunop (_, e) -> analyze_expr e env
    | Ebinop (e1, _, e2) ->
      let env = analyze_expr e1 env in
      analyze_expr e2 env
    | Evariadic -> env
    | Efunctiondef (_, block) ->
      let _ = analyze_block block env in
      env
    | Eprefix prefixexp -> analyze_prefixexp prefixexp env
    | Etableconstructor fl ->
      List.fold_left (fun env field -> analyze_field field env) env fl

  and analyze_prefixexp prefixexp env =
    match prefixexp with
    | PEvar _ | PEfunctioncall _ -> env
    | PEexp e -> analyze_expr e env

  and analyze_field field env =
    match field with
    | Fexp e | Fname (_, e) -> analyze_expr e env
    | Fcol (e1, e2) ->
      let env = analyze_expr e1 env in
      analyze_expr e2 env

  and analyze_stmt stmt env =
    match stmt with
    | Sassign (vl, _) ->
      List.iter
        (fun var ->
          match var with
          | VarName name -> begin
            match SMap.find_opt name env with
            | Some true ->
              error None
                (Format.sprintf "attempt to assign to const variable '%s'" name)
            | Some false -> ()
            | None -> ()
          end
          | VarTableField _ -> () )
        vl;
      env
    | SassignLocal (vl, _) ->
      List.fold_left
        (fun env (name, attrib_opt) ->
          match attrib_opt with
          | Some attrib -> SMap.add name (is_const_var attrib) env
          | None -> SMap.add name false env )
        env vl
    | Sempty | Sbreak | Sreturn _ | Slabel _ | Sgoto _ | SfunctionCall _ -> env
    | Sblock b | Swhile (_, b) | Srepeat (b, _) ->
      let _ = analyze_block b env in
      env
    | Sif (e, b, ebl, ob) -> (
      let env = analyze_expr e env in
      let _ = analyze_block b env in
      let _ =
        List.fold_left
          (fun env (e, b) ->
            let env = analyze_expr e env in
            analyze_block b env )
          env ebl
      in
      match ob with
      | None -> env
      | Some b ->
        let _ = analyze_block b env in
        env )
    | Sfor (_, e1, e2, oe, b) ->
      let env = analyze_expr e1 env in
      let env = analyze_expr e2 env in
      let env = match oe with None -> env | Some e -> analyze_expr e env in
      let _ = analyze_block b env in
      env
    | Siterator (_, el, b) ->
      let env = List.fold_left (fun env exp -> analyze_expr exp env) env el in
      let _ = analyze_block b env in
      env

  and analyze_block block env =
    List.fold_left (fun env stmt -> analyze_stmt stmt env) env block

  let analyze chunk =
    try
      let env = SMap.empty in
      let _ = analyze_block chunk env in
      Ok ()
    with Static_analysis_error (loc, message) -> error loc message
end
