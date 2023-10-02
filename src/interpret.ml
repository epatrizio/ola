open Ast

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
  | Evalue (Vnil _) -> Vnil ()
  | Evalue (Vboolean b) -> Vboolean b
  | Evalue (Vnumber (Ninteger i)) -> Vnumber (Ninteger i)
  | Evalue (Vnumber (Nfloat f)) -> Vnumber (Nfloat f)
  | Evalue (Vstring s) -> Vstring s
  | Evar _v -> Vnil () (* todo: to be implemented *)
  | Eunop (Unot, e) ->
    let v = interpret_expr e in
    begin
      match v with
      | Vboolean b -> Vboolean (not b)
      | _ -> assert false (* typing error *)
    end
  | Eunop (Uminus, e) ->
    let v = interpret_expr e in
    begin
      match v with
      | Vnumber (Ninteger i) -> Vnumber (Ninteger (-i))
      | Vnumber (Nfloat f) -> Vnumber (Nfloat (-.f))
      | _ -> assert false (* typing error *)
    end
  | Eunop (Usharp, e) ->
    let v = interpret_expr e in
    begin
      match v with
      | Vstring s -> Vnumber (Ninteger (String.length s))
      | _ -> assert false (* typing error *)
    end
  | Ebinop (Band, e1, e2) -> interpret_bbinop_expr Band e1 e2
  | Ebinop (Bor, e1, e2) -> interpret_bbinop_expr Bor e1 e2
  | Ebinop (Badd, e1, e2) -> interpret_ibinop_expr Badd e1 e2
  | Ebinop (Bsub, e1, e2) -> interpret_ibinop_expr Bsub e1 e2
  | Ebinop (Bmul, e1, e2) -> interpret_ibinop_expr Bmul e1 e2
  | Ebinop (Bdiv, e1, e2) -> interpret_ibinop_expr Bdiv e1 e2
  | Ebinop (Bfldiv, e1, e2) -> interpret_ibinop_expr Bfldiv e1 e2
  | Ebinop (Bmod, e1, e2) -> interpret_ibinop_expr Bmod e1 e2
  | Ebinop (Bexp, e1, e2) -> interpret_ibinop_expr Bexp e1 e2
  | Ebinop (Blt, e1, e2) -> interpret_ibinop_expr Blt e1 e2
  | Ebinop (Ble, e1, e2) -> interpret_ibinop_expr Ble e1 e2
  | Ebinop (Bgt, e1, e2) -> interpret_ibinop_expr Bgt e1 e2
  | Ebinop (Bge, e1, e2) -> interpret_ibinop_expr Bge e1 e2
  | Ebinop (Beq, e1, e2) -> interpret_ibinop_expr Beq e1 e2
  | Ebinop (Bneq, e1, e2) -> interpret_ibinop_expr Bneq e1 e2
  | Ebinop (Bddot, e1, e2) -> interpret_sbinop_expr e1 e2

let rec interpret_stmt stmt =
  match stmt with
  | Sempty -> ()
  | Sassign (_il, _el) -> () (* todo: to be implemented *)
  | Slabel _n -> () (* todo: to be implemented *)
  | Sgoto _n -> () (* todo: to be implemented *)
  | Sblock b -> interpret_block b
  | Swhile (e, b) ->
    let v = interpret_expr e in
    begin
      match v with
      | Vboolean cond ->
        if cond then begin
          interpret_block b;
          interpret_stmt (Swhile (e, b))
        end
      | _ -> assert false (* typing error *)
    end
  | Srepeat (b, e) ->
    let v = interpret_expr e in
    begin
      match v with
      | Vboolean cond ->
        interpret_block b;
        if not cond then interpret_stmt (Srepeat (b, e))
      | _ -> assert false (* typing error *)
    end
  | Sif (_e, _b, _ebl, _ob) -> () (* todo: to be implemented *)
  | Sprint e ->
    print_value Format.std_formatter (interpret_expr e);
    Format.fprintf Format.std_formatter "@."

and interpret_block b = List.iter interpret_stmt b

let run chunk = interpret_block chunk
