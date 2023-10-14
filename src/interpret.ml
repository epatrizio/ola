open Ast

type block_pointer =
  | Begin
  | Last
  | Label of string

exception Goto_catch of block_pointer * Env.t

exception Interpretation_error of location option * string

let error loc message = raise (Interpretation_error (loc, message))

let rec block_from_pointer pt stmt_list =
  match (pt, stmt_list) with
  | Begin, _ -> stmt_list
  | Last, [] -> stmt_list
  | Last, [ _stmt ] -> stmt_list
  | Last, _stmt :: tl -> block_from_pointer Last tl
  | Label l, [] -> error None ("no visible label for <goto> " ^ l)
  | Label l, Slabel n :: tl when l = n -> tl
  | Label l, _stmt :: tl -> block_from_pointer (Label l) tl

let rec interpret_expr expr env =
  let get_int f loc =
    begin
      match Float.is_integer f with
      | true -> int_of_float f
      | false ->
        error (Some loc)
          ("number has no integer representation: " ^ string_of_float f)
    end
  in
  let interpret_bbinop_expr binop expr1 expr2 env =
    (* todo: ok for num values *)
    let v1, env = interpret_expr expr1 env in
    let v2, env = interpret_expr expr2 env in
    begin
      match (v1, v2) with
      | Vboolean b1, Vboolean b2 -> begin
        match binop with
        | Band -> (Vboolean (b1 && b2), env)
        | Bor -> (Vboolean (b1 || b2), env)
        | _ -> assert false (* call error *)
      end
      | _ -> assert false (* typing error *)
    end
  in
  let interpret_ibinop_expr binop expr1 expr2 env =
    let loc1, _expr1 = expr1 in
    let loc2, _expr2 = expr2 in
    let v1, env = interpret_expr expr1 env in
    let v2, env = interpret_expr expr2 env in
    begin
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
        | Bland ->
          (Vnumber (Ninteger (get_int f1 loc1 land get_int f2 loc2)), env)
        | Blor -> (Vnumber (Ninteger (get_int f1 loc1 lor get_int f2 loc2)), env)
        | Blxor ->
          (Vnumber (Ninteger (get_int f1 loc1 lxor get_int f2 loc2)), env)
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
    end
  in
  let interpret_sbinop_expr expr1 expr2 env =
    let v1, env = interpret_expr expr1 env in
    let v2, env = interpret_expr expr2 env in
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
  in
  match expr with
  | _loc, Evalue (Vnil _) -> (Vnil (), env)
  | _loc, Evalue (Vboolean b) -> (Vboolean b, env)
  | _loc, Evalue (Vnumber (Ninteger i)) -> (Vnumber (Ninteger i), env)
  | _loc, Evalue (Vnumber (Nfloat f)) -> (Vnumber (Nfloat f), env)
  | _loc, Evalue (Vstring s) -> (Vstring s, env)
  | _loc, Evar (Name n) -> (Env.get_value n env, env)
  | _loc, Eunop (Unot, e) ->
    let v, env = interpret_expr e env in
    begin
      match v with
      | Vboolean b -> (Vboolean (not b), env)
      | _ -> assert false (* typing error *)
    end
  | _loc, Eunop (Uminus, e) ->
    let v, env = interpret_expr e env in
    begin
      match v with
      | Vnumber (Ninteger i) -> (Vnumber (Ninteger (-i)), env)
      | Vnumber (Nfloat f) -> (Vnumber (Nfloat (-.f)), env)
      | _ -> assert false (* typing error *)
    end
  | _loc, Eunop (Usharp, e) ->
    let v, env = interpret_expr e env in
    begin
      match v with
      | Vstring s -> (Vnumber (Ninteger (String.length s)), env)
      | _ -> assert false (* typing error *)
    end
  | loc, Eunop (Ulnot, e) ->
    let v, env = interpret_expr e env in
    begin
      match v with
      | Vnumber (Ninteger i) -> (Vnumber (Ninteger (lnot i)), env)
      | Vnumber (Nfloat f) -> (Vnumber (Ninteger (lnot (get_int f loc))), env)
      | _ -> assert false (* typing error *)
    end
  | _loc, Ebinop (Band, e1, e2) -> interpret_bbinop_expr Band e1 e2 env
  | _loc, Ebinop (Bor, e1, e2) -> interpret_bbinop_expr Bor e1 e2 env
  | _loc, Ebinop (Badd, e1, e2) -> interpret_ibinop_expr Badd e1 e2 env
  | _loc, Ebinop (Bsub, e1, e2) -> interpret_ibinop_expr Bsub e1 e2 env
  | _loc, Ebinop (Bmul, e1, e2) -> interpret_ibinop_expr Bmul e1 e2 env
  | _loc, Ebinop (Bdiv, e1, e2) -> interpret_ibinop_expr Bdiv e1 e2 env
  | _loc, Ebinop (Bfldiv, e1, e2) -> interpret_ibinop_expr Bfldiv e1 e2 env
  | _loc, Ebinop (Bmod, e1, e2) -> interpret_ibinop_expr Bmod e1 e2 env
  | _loc, Ebinop (Bexp, e1, e2) -> interpret_ibinop_expr Bexp e1 e2 env
  | _loc, Ebinop (Bland, e1, e2) -> interpret_ibinop_expr Bland e1 e2 env
  | _loc, Ebinop (Blor, e1, e2) -> interpret_ibinop_expr Blor e1 e2 env
  | _loc, Ebinop (Blxor, e1, e2) -> interpret_ibinop_expr Blxor e1 e2 env
  | _loc, Ebinop (Blsl, e1, e2) -> interpret_ibinop_expr Blsl e1 e2 env
  | _loc, Ebinop (Blsr, e1, e2) -> interpret_ibinop_expr Blsr e1 e2 env
  | _loc, Ebinop (Blt, e1, e2) -> interpret_ibinop_expr Blt e1 e2 env
  | _loc, Ebinop (Ble, e1, e2) -> interpret_ibinop_expr Ble e1 e2 env
  | _loc, Ebinop (Bgt, e1, e2) -> interpret_ibinop_expr Bgt e1 e2 env
  | _loc, Ebinop (Bge, e1, e2) -> interpret_ibinop_expr Bge e1 e2 env
  | _loc, Ebinop (Beq, e1, e2) -> interpret_ibinop_expr Beq e1 e2 env
  | _loc, Ebinop (Bneq, e1, e2) -> interpret_ibinop_expr Bneq e1 e2 env
  | _loc, Ebinop (Bddot, e1, e2) -> interpret_sbinop_expr e1 e2 env

let rec interpret_stmt stmt env =
  let rec lists_assign vl el env =
    begin
      match (vl, el) with
      | [], [] | [], _ -> env
      | vl, [] ->
        List.fold_left
          (fun e v ->
            let (Name n) = v in
            Env.set_value n (Ast.Vnil ()) e )
          env vl
      | v :: vl, e :: el ->
        let (Name n) = v in
        let va, env = interpret_expr e env in
        let env = Env.set_value n va env in
        lists_assign vl el env
    end
  in
  match stmt with
  | Sempty -> env
  | Sassign (vl, el) -> lists_assign vl el env
  | SassignLocal (_nal, _elo) -> env (* todo: to be implemented *)
  | Sbreak -> env (* todo: to be implemented *)
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
      | _ ->
        let env = interpret_block b env in
        interpret_stmt (Swhile (e, b)) env
    end
  | Srepeat (b, e) ->
    let env = interpret_block b env in
    let cond, env = interpret_expr e env in
    begin
      match cond with
      | Vboolean false | Vnil () -> interpret_stmt (Srepeat (b, e)) env
      | _ -> env
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
  | Sfor (_n, e1, e2, oe, b) ->
    (* TODO refacto implem *)
    (* todo: need local environment for _n *)
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
    let interpret_cond exp1 exp2 step env =
      let op =
        match step with
        | Vnumber (Ninteger i) -> if i >= 0 then Ble else Bge
        | Vnumber (Nfloat f) -> if f >= 0. then Ble else Bge
        | _ -> assert false (* call error *)
      in
      let loc1, _e1 = exp1 in
      let v, env = interpret_expr (loc1, Ebinop (op, exp1, exp2)) env in
      match v with Vboolean b -> (b, env) | _ -> assert false (* call error *)
    in
    let incr_init initial step env =
      let loc, _e = initial in
      let v, env = interpret_expr (loc, Ebinop (Badd, initial, step)) env in
      match v with
      | Vnumber (Ninteger i) -> (Vnumber (Ninteger i), env)
      | Vnumber (Nfloat f) -> (Vnumber (Nfloat f), env)
      | _ -> assert false (* call error *)
    in
    let l1, _e1 = e1 in
    let l2, _e2 = e2 in
    let ival, env = init_val e1 env in
    let initial = ref ival in
    let limit, env = init_val e2 env in
    let step, env =
      match oe with
      | Some e -> init_val e env
      | None -> (Vnumber (Ninteger 1), env)
    in
    let cond = ref false in
    (* initial <= limit *)
    let c, env =
      interpret_cond (l1, Evalue !initial) (l2, Evalue limit) step env
    in
    cond := c;
    while !cond do
      let env = interpret_block b env in
      let i, _env = incr_init (l1, Evalue !initial) (l1, Evalue step) env in
      initial := i;
      let c, _env =
        interpret_cond (l1, Evalue !initial) (l2, Evalue limit) step env
      in
      cond := c
    done;
    env
  | Siterator (_nl, _el, _b) -> env (* todo: to be implemented *)
  | Sprint e ->
    let v, _env = interpret_expr e env in
    print_value Format.std_formatter v;
    Format.fprintf Format.std_formatter "@.";
    env

and interpret_block b env =
  List.fold_left (fun e stmt -> interpret_stmt stmt e) env b

let rec run ?(pt = Begin) chunk env =
  try
    let bl = block_from_pointer pt chunk in
    interpret_block bl env
  with Goto_catch (label, env) -> run ~pt:label chunk env
