open Ast

exception Goto_catch of string

exception Interpretation_error of location option * string

let error loc message = raise (Interpretation_error (loc, message))

let rec block_from_label label stmt_list =
  match (label, stmt_list) with
  | None, _ -> stmt_list
  | Some lab, [] -> error None ("no visible label for <goto> " ^ lab)
  | Some lab, Slabel n :: tl when lab = n -> tl
  | Some _, _stmt :: tl -> block_from_label label tl

let rec interpret_expr expr =
  let interpret_bbinop_expr binop expr1 expr2 =
    (* todo: ok for num values *)
    let v1 = interpret_expr expr1 in
    let v2 = interpret_expr expr2 in
    begin
      match (v1, v2) with
      | Vboolean b1, Vboolean b2 -> begin
        match binop with
        | Band -> Vboolean (b1 && b2)
        | Bor -> Vboolean (b1 || b2)
        | _ -> assert false (* call error *)
      end
      | _ -> assert false (* typing error *)
    end
  in
  let interpret_ibinop_expr binop expr1 expr2 =
    let v1 = interpret_expr expr1 in
    let v2 = interpret_expr expr2 in
    begin
      match (v1, v2) with
      | Vnumber (Ninteger i1), Vnumber (Ninteger i2) -> begin
        match binop with
        | Badd -> Vnumber (Ninteger (i1 + i2))
        | Bsub -> Vnumber (Ninteger (i1 - i2))
        | Bmul -> Vnumber (Ninteger (i1 * i2))
        | Bdiv -> Vnumber (Nfloat (float_of_int i1 /. float_of_int i2))
        | Bfldiv -> Vnumber (Ninteger (i1 / i2)) (* todo div by 0 error *)
        | Bmod -> Vnumber (Ninteger (i1 mod i2))
        | Bexp ->
          Vnumber (Nfloat (Float.pow (float_of_int i1) (float_of_int i2)))
        | Blt -> Vboolean (i1 < i2)
        | Ble -> Vboolean (i1 <= i2)
        | Bgt -> Vboolean (i1 > i2)
        | Bge -> Vboolean (i1 >= i2)
        | Beq -> Vboolean (i1 = i2)
        | Bneq -> Vboolean (i1 != i2)
        | _ -> assert false (* call error *)
      end
      | Vnumber (Nfloat f), Vnumber (Ninteger i) -> begin
        match binop with
        | Badd -> Vnumber (Nfloat (f +. float_of_int i))
        | Bsub -> Vnumber (Nfloat (f -. float_of_int i))
        | Bmul -> Vnumber (Nfloat (f *. float_of_int i))
        | Bdiv -> Vnumber (Nfloat (f /. float_of_int i))
        | Bfldiv ->
          let _, q = Float.modf (f /. float_of_int i) in
          Vnumber (Nfloat q)
        | Bmod ->
          let fi = float_of_int i in
          let _, q = Float.modf (f /. fi) in
          Vnumber (Nfloat (f -. (fi *. q)))
        | Bexp -> Vnumber (Nfloat (Float.pow f (float_of_int i)))
        | Blt -> Vboolean (f < float_of_int i)
        | Ble -> Vboolean (f <= float_of_int i)
        | Bgt -> Vboolean (f > float_of_int i)
        | Bge -> Vboolean (f >= float_of_int i)
        | Beq -> Vboolean (f = float_of_int i)
        | Bneq -> Vboolean (f != float_of_int i)
        | _ -> assert false (* call error *)
      end
      | Vnumber (Ninteger i), Vnumber (Nfloat f) -> begin
        match binop with
        | Badd -> Vnumber (Nfloat (float_of_int i +. f))
        | Bsub -> Vnumber (Nfloat (float_of_int i -. f))
        | Bmul -> Vnumber (Nfloat (float_of_int i *. f))
        | Bdiv -> Vnumber (Nfloat (float_of_int i /. f))
        | Bfldiv ->
          let _, q = Float.modf (float_of_int i /. f) in
          Vnumber (Nfloat q)
        | Bmod ->
          let fi = float_of_int i in
          let _, q = Float.modf (fi /. f) in
          Vnumber (Nfloat (fi -. (f *. q)))
        | Bexp -> Vnumber (Nfloat (Float.pow (float_of_int i) f))
        | Blt -> Vboolean (float_of_int i < f)
        | Ble -> Vboolean (float_of_int i <= f)
        | Bgt -> Vboolean (float_of_int i > f)
        | Bge -> Vboolean (float_of_int i >= f)
        | Beq -> Vboolean (float_of_int i = f)
        | Bneq -> Vboolean (float_of_int i != f)
        | _ -> assert false (* call error *)
      end
      | Vnumber (Nfloat f1), Vnumber (Nfloat f2) -> begin
        match binop with
        | Badd -> Vnumber (Nfloat (f1 +. f2))
        | Bsub -> Vnumber (Nfloat (f1 -. f2))
        | Bmul -> Vnumber (Nfloat (f1 *. f2))
        | Bdiv -> Vnumber (Nfloat (f1 /. f2))
        | Bfldiv ->
          let _, q = Float.modf (f1 /. f2) in
          Vnumber (Nfloat q)
        | Bmod ->
          let _, q = Float.modf (f1 /. f2) in
          Vnumber (Nfloat (f1 -. (f2 *. q)))
        | Bexp -> Vnumber (Nfloat (Float.pow f1 f2))
        | Blt -> Vboolean (f1 < f2)
        | Ble -> Vboolean (f1 <= f2)
        | Bgt -> Vboolean (f1 > f2)
        | Bge -> Vboolean (f1 >= f2)
        | Beq -> Vboolean (f1 = f2)
        | Bneq -> Vboolean (f1 != f2)
        | _ -> assert false (* call error *)
      end
      | _ -> assert false (* typing error *)
    end
  in
  let interpret_sbinop_expr expr1 expr2 =
    let v1 = interpret_expr expr1 in
    let v2 = interpret_expr expr2 in
    begin
      match (v1, v2) with
      | Vstring s1, Vstring s2 -> Vstring (s1 ^ s2)
      | Vstring s, Vnumber (Ninteger i) -> Vstring (s ^ string_of_int i)
      | Vstring s, Vnumber (Nfloat f) -> Vstring (s ^ string_of_float f)
      | Vnumber (Ninteger i), Vstring s -> Vstring (string_of_int i ^ s)
      | Vnumber (Nfloat f), Vstring s -> Vstring (string_of_float f ^ s)
      | Vnumber (Ninteger i1), Vnumber (Ninteger i2) ->
        Vstring (string_of_int i1 ^ string_of_int i2)
      | Vnumber (Nfloat f1), Vnumber (Nfloat f2) ->
        Vstring (string_of_float f1 ^ string_of_float f2)
      | Vnumber (Ninteger i), Vnumber (Nfloat f) ->
        Vstring (string_of_int i ^ string_of_float f)
      | Vnumber (Nfloat f), Vnumber (Ninteger i) ->
        Vstring (string_of_float f ^ string_of_int i)
      | _ -> assert false (* typing error *)
    end
  in
  match expr with
  | _loc, Evalue (Vnil _) -> Vnil ()
  | _loc, Evalue (Vboolean b) -> Vboolean b
  | _loc, Evalue (Vnumber (Ninteger i)) -> Vnumber (Ninteger i)
  | _loc, Evalue (Vnumber (Nfloat f)) -> Vnumber (Nfloat f)
  | _loc, Evalue (Vstring s) -> Vstring s
  | _loc, Evar _v -> Vnil () (* todo: to be implemented *)
  | _loc, Eunop (Unot, e) ->
    let v = interpret_expr e in
    begin
      match v with
      | Vboolean b -> Vboolean (not b)
      | _ -> assert false (* typing error *)
    end
  | _loc, Eunop (Uminus, e) ->
    let v = interpret_expr e in
    begin
      match v with
      | Vnumber (Ninteger i) -> Vnumber (Ninteger (-i))
      | Vnumber (Nfloat f) -> Vnumber (Nfloat (-.f))
      | _ -> assert false (* typing error *)
    end
  | _loc, Eunop (Usharp, e) ->
    let v = interpret_expr e in
    begin
      match v with
      | Vstring s -> Vnumber (Ninteger (String.length s))
      | _ -> assert false (* typing error *)
    end
  | _loc, Ebinop (Band, e1, e2) -> interpret_bbinop_expr Band e1 e2
  | _loc, Ebinop (Bor, e1, e2) -> interpret_bbinop_expr Bor e1 e2
  | _loc, Ebinop (Badd, e1, e2) -> interpret_ibinop_expr Badd e1 e2
  | _loc, Ebinop (Bsub, e1, e2) -> interpret_ibinop_expr Bsub e1 e2
  | _loc, Ebinop (Bmul, e1, e2) -> interpret_ibinop_expr Bmul e1 e2
  | _loc, Ebinop (Bdiv, e1, e2) -> interpret_ibinop_expr Bdiv e1 e2
  | _loc, Ebinop (Bfldiv, e1, e2) -> interpret_ibinop_expr Bfldiv e1 e2
  | _loc, Ebinop (Bmod, e1, e2) -> interpret_ibinop_expr Bmod e1 e2
  | _loc, Ebinop (Bexp, e1, e2) -> interpret_ibinop_expr Bexp e1 e2
  | _loc, Ebinop (Blt, e1, e2) -> interpret_ibinop_expr Blt e1 e2
  | _loc, Ebinop (Ble, e1, e2) -> interpret_ibinop_expr Ble e1 e2
  | _loc, Ebinop (Bgt, e1, e2) -> interpret_ibinop_expr Bgt e1 e2
  | _loc, Ebinop (Bge, e1, e2) -> interpret_ibinop_expr Bge e1 e2
  | _loc, Ebinop (Beq, e1, e2) -> interpret_ibinop_expr Beq e1 e2
  | _loc, Ebinop (Bneq, e1, e2) -> interpret_ibinop_expr Bneq e1 e2
  | _loc, Ebinop (Bddot, e1, e2) -> interpret_sbinop_expr e1 e2

let rec interpret_stmt stmt =
  match stmt with
  | Sempty -> ()
  | Sassign (_il, _el) -> () (* todo: to be implemented *)
  | Sbreak -> () (* todo: to be implemented *)
  | Slabel _ -> ()
  | Sgoto n -> raise (Goto_catch n)
  | Sblock b -> interpret_block b
  | Swhile (e, b) ->
    (* Doc: The condition expression of a control structure can return any value.
       Both false and nil test false. *)
    let cond = interpret_expr e in
    begin
      match cond with
      | Vboolean false | Vnil () -> ()
      | _ ->
        interpret_block b;
        interpret_stmt (Swhile (e, b))
    end
  | Srepeat (b, e) ->
    interpret_block b;
    let cond = interpret_expr e in
    begin
      match cond with
      | Vboolean false | Vnil () -> interpret_stmt (Srepeat (b, e))
      | _ -> ()
    end
  | Sif (e, b, ebl, ob) ->
    let rec interpret_elseif ebl =
      match ebl with
      | [] -> None
      | (e, b) :: tl ->
        let cond = interpret_expr e in
        begin
          match cond with
          | Vboolean false | Vnil () -> interpret_elseif tl
          | _ -> Some (interpret_block b)
        end
    in
    let cond = interpret_expr e in
    begin
      match cond with
      | Vboolean false | Vnil () -> begin
        match interpret_elseif ebl with
        | Some () -> ()
        | None -> begin
          match ob with Some b -> interpret_block b | None -> ()
        end
      end
      | _ -> interpret_block b
    end
  | Sfor (_n, e1, e2, oe, b) ->
    (* todo: environment for _n *)
    let init_val expr =
      match interpret_expr expr with
      | Vnumber (Ninteger i) -> Vnumber (Ninteger i)
      | Vnumber (Nfloat f) -> Vnumber (Nfloat f)
      | Vstring s -> begin
        match float_of_string_opt s with
        | Some f -> Vnumber (Nfloat f)
        | None -> assert false (* typing error *)
      end
      | _ -> assert false (* typing error *)
    in
    let interpret_cond exp1 exp2 step =
      let op =
        match step with
        | Vnumber (Ninteger i) -> if i >= 0 then Ble else Bge
        | Vnumber (Nfloat f) -> if f >= 0. then Ble else Bge
        | _ -> assert false (* call error *)
      in
      let loc1, _e1 = exp1 in
      match interpret_expr (loc1, Ebinop (op, exp1, exp2)) with
      | Vboolean b -> b
      | _ -> assert false (* call error *)
    in
    let incr_init initial step =
      let loc, _e = initial in
      match interpret_expr (loc, Ebinop (Badd, initial, step)) with
      | Vnumber (Ninteger i) -> Vnumber (Ninteger i)
      | Vnumber (Nfloat f) -> Vnumber (Nfloat f)
      | _ -> assert false (* call error *)
    in
    let l1, _e1 = e1 in
    let l2, _e2 = e2 in
    let initial = ref (init_val e1) in
    let limit = init_val e2 in
    let step =
      match oe with Some e -> init_val e | None -> Vnumber (Ninteger 1)
    in
    while interpret_cond (l1, Evalue !initial) (l2, Evalue limit) step do
      (* initial <= limit *)
      interpret_block b;
      initial := incr_init (l1, Evalue !initial) (l1, Evalue step)
    done
  | Siterator (_nl, _el, _b) -> () (* todo: to be implemented *)
  | Sprint e ->
    print_value Format.std_formatter (interpret_expr e);
    Format.fprintf Format.std_formatter "@."

and interpret_block b = List.iter interpret_stmt b

let rec run ?(label = None) chunk =
  try
    let bl = block_from_label label chunk in
    interpret_block bl
  with Goto_catch label -> run ~label:(Some label) chunk
