(* Static analysis:
    Check if a function definition is variadic.
    If so, check if the use of Evariadic expression is permitted in the function body
  *)

open Ast
open Utils

exception Static_analysis_error of Ast.location * string

let error loc message = raise (Static_analysis_error (loc, message))

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
        error loc "cannot use '...' outside a vararg function near '...'"
      else false

  and analyze_prefixexp prefixexp =
    match prefixexp with
    | PEvar _ -> false
    | PEfunctioncall _ -> false
    | PEexp e -> analyze_expr e

  and analyze_field field =
    match field with
    | Fexp e -> analyze_expr e
    | Fname (_, e) -> analyze_expr e
    | Fcol (e1, e2) -> analyze_expr e1 || analyze_expr e2

  and analyze_stmt stmt =
    match stmt with
    | Sempty -> false
    | Sassign (_, el) | SassignLocal (_, el) -> check_list el analyze_expr
    | Sbreak -> false
    | Sreturn _ -> false
    | Slabel _ -> false
    | Sgoto _ -> false
    | Sblock b -> analyze_block b
    | Swhile (_, b) -> analyze_block b
    | Srepeat (b, _) -> analyze_block b
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
    | SfunctionCall _ -> false

  and analyze_block block = check_list block analyze_stmt

  let analyze chunk =
    try
      let _ = analyze_block chunk in
      Ok ()
    with Static_analysis_error (loc, message) -> error loc message
end
