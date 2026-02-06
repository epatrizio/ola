open Ast
open Ast.Value
open Syntax
open Typer

exception Evaluation_error of location option * string

let error loc_opt message = raise (Evaluation_error (loc_opt, message))

module Eval_utils : sig
  val number_of_string : location option -> Ast.Value.t -> Ast.Value.t

  val integer_of_float : location -> Ast.Value.t -> Ast.Value.t

  val integer_of_float_value : Ast.Value.t -> Ast.Value.t
end = struct
  let number_of_string loc value =
    match value with
    | Vstring str -> begin
      match int_of_string_opt str with
      | Some i -> Vnumber (Ninteger i)
      | None -> (
        match float_of_string_opt str with
        | Some f -> Vnumber (Nfloat f)
        | None ->
          error loc
            (Format.sprintf "attempt to perform on a string (%s) value" str) )
    end
    | _ -> value

  let integer_of_float loc value =
    match value with
    | Vnumber (Nfloat f) ->
      if Float.is_integer f then Vnumber (Ninteger (int_of_float f))
      else
        error (Some loc)
          ("number has no integer representation: " ^ string_of_float f)
    | _ -> value

  let integer_of_float_value = function
    | Vnumber (Nfloat f) as v ->
      if Float.is_integer f then Vnumber (Ninteger (int_of_float f)) else v
    | v -> v
end

open Eval_utils

let eval_unop unop (loc, v) env =
  match unop with
  | Unot ->
    let* _ = typecheck_expr (loc, Eunop (Unot, (loc, Evalue v))) env in
    begin match v with
    | Vnil () -> Ok (Vboolean true, env)
    | Vboolean b -> Ok (Vboolean (not b), env)
    | _ -> Ok (Vboolean false, env)
    end
  | Uminus ->
    let v = number_of_string (Some loc) v in
    let* _ = typecheck_expr (loc, Eunop (Uminus, (loc, Evalue v))) env in
    begin match v with
    | Vnumber (Ninteger i) -> Ok (Vnumber (Ninteger (-i)), env)
    | Vnumber (Nfloat f) -> Ok (Vnumber (Nfloat (-.f)), env)
    | _ -> assert false (* typing error *)
    end
  | Usharp ->
    let* _ = typecheck_expr (loc, Eunop (Usharp, (loc, Evalue v))) env in
    begin match v with
    | Vstring s -> Ok (Vnumber (Ninteger (String.length s)), env)
    | Vtable (_i, t) ->
      Ok (Vnumber (Ninteger (LuaTable.border (fun v -> v = Vnil ()) t)), env)
    | _ -> assert false (* typing error *)
    end
  | Ulnot ->
    let v = integer_of_float loc v in
    let* _ = typecheck_expr (loc, Eunop (Ulnot, (loc, Evalue v))) env in
    begin match v with
    | Vnumber (Ninteger i) -> Ok (Vnumber (Ninteger (lnot i)), env)
    | _ -> assert false (* typing error *)
    end

let eval_arith_binop binop (loc1, v1) (loc2, v2) env =
  let v1 = number_of_string (Some loc1) v1 in
  let v2 = number_of_string (Some loc2) v2 in
  let* _ =
    typecheck_expr
      (loc1, Ebinop ((loc1, Evalue v1), binop, (loc2, Evalue v2)))
      env
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

let eval_bitwise_binop binop (loc1, v1) (loc2, v2) env =
  let v1 = integer_of_float loc1 v1 in
  let v2 = integer_of_float loc1 v2 in
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
  | _ -> assert false (* typing error *)

let eval_rel_binop binop (loc1, v1) (loc2, v2) env =
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
  | _ -> assert false
(* todo: to be implemented (Ex. VfunctionReturn. Check Array.lua example) *)

let eval_str_binop (loc1, v1) (loc2, v2) env =
  let* _ =
    typecheck_expr
      (loc1, Ebinop ((loc1, Evalue v1), Bddot, (loc2, Evalue v2)))
      env
  in
  begin match (v1, v2) with
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

let eval_binop binop (loc1, v1) (loc2, v2) env =
  match binop with
  | Bddot -> eval_str_binop (loc1, v1) (loc2, v2) env
  | Badd | Bsub | Bmul | Bdiv | Bfldiv | Bmod | Bexp ->
    eval_arith_binop binop (loc1, v1) (loc2, v2) env
  | Bland | Blor | Blxor | Blsl | Blsr ->
    eval_bitwise_binop binop (loc1, v1) (loc2, v2) env
  | Blt | Ble | Bgt | Bge | Beq | Bneq ->
    eval_rel_binop binop (loc1, v1) (loc2, v2) env
  | Band | Bor ->
    (* call error - short-circuit eval in Interpret module *)
    assert false

(* wip
let eval_bbinop binop v1 v2 =
  match binop with
  | Band -> begin
    match v1 with
    | Vboolean false | Vnil () -> v1
    | _ -> v2 (* WIP: short-circuit evaluation *)
  end
  | Bor -> begin
    match v1 with
    | v when v <> Vnil () && v <> Vboolean false -> v1
    | _ -> v2 (* WIP: short-circuit evaluation *)
  end
  | _ -> assert false call error *)
