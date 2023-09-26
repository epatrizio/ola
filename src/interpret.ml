open Ast

let rec interpret_expr expr =
  let interpret_bbinop_expr binop expr1 expr2 =
    let v1 = interpret_expr expr1 in
    let v2 = interpret_expr expr2 in begin
      match v1, v2 with
      | Vboolean b1, Vboolean b2 ->
        begin match binop with
          | Band -> Vboolean (b1 && b2)
          | Bor -> Vboolean (b1 || b2)
          | _ -> assert false (* call error *)
        end
      | _ -> assert false (* typing error *)
    end
  in
  let interpret_ibinop_expr binop expr1 expr2 =
    let v1 = interpret_expr expr1 in
    let v2 = interpret_expr expr2 in begin
      match v1, v2 with
      | Vnumber (Ninteger i1), Vnumber (Ninteger i2) ->
        begin match binop with
          | Badd -> Vnumber (Ninteger (i1 + i2))
          | Bsub -> Vnumber (Ninteger (i1 - i2))
          | Bmul -> Vnumber (Ninteger (i1 * i2))
          | Blt -> Vboolean (i1 < i2)
          | Ble -> Vboolean (i1 <= i2)
          | Bgt -> Vboolean (i1 > i2)
          | Bge -> Vboolean (i1 >= i2)
          | Beq -> Vboolean (i1 == i2)
          | Bneq -> Vboolean (i1 != i2)
          | _ -> assert false (* call error *)
        end
      | _ -> assert false (* typing error *)
    end
  in
  match expr with
  | Evalue Vnil _ -> Vnil ()
  | Evalue Vboolean b -> Vboolean b
  | Evalue Vnumber Ninteger i -> Vnumber (Ninteger i)
  | Evalue Vnumber Nfloat f -> Vnumber (Nfloat f)
  | Evalue _ -> assert false
  | Eident _i -> assert false
  | Eunop (Unot, e) ->
    let v = interpret_expr e in begin
      match v with
      | Vboolean b -> Vboolean (not b)
      | _ -> assert false (* typing error *)
    end
  | Eunop (Uminus, e) ->
    let v = interpret_expr e in begin
      match v with
      | Vnumber (Ninteger i) -> Vnumber (Ninteger (-i))
      | _ -> assert false (* typing error *)
    end
  | Ebinop (Band, e1, e2) -> interpret_bbinop_expr Band e1 e2
  | Ebinop (Bor, e1, e2) -> interpret_bbinop_expr Bor e1 e2
  | Ebinop (Badd, e1, e2) -> interpret_ibinop_expr Badd e1 e2
  | Ebinop (Bsub, e1, e2) -> interpret_ibinop_expr Bsub e1 e2
  | Ebinop (Bmul, e1, e2) -> interpret_ibinop_expr Bmul e1 e2
  | Ebinop (Blt, e1, e2) -> interpret_ibinop_expr Blt e1 e2
  | Ebinop (Ble, e1, e2) -> interpret_ibinop_expr Ble e1 e2
  | Ebinop (Bgt, e1, e2) -> interpret_ibinop_expr Bgt e1 e2
  | Ebinop (Bge, e1, e2) -> interpret_ibinop_expr Bge e1 e2
  | Ebinop (Beq, e1, e2) -> interpret_ibinop_expr Beq e1 e2
  | Ebinop (Bneq, e1, e2) -> interpret_ibinop_expr Bneq e1 e2

let rec interpret_stmt stmt =
  match stmt with
  | Sempty -> ()
  | Sblock b -> interpret_block b
  | Swhile (e, b) ->
    let v = interpret_expr e in begin
      match v with
      | Vboolean cond -> if cond then interpret_block b else ()
      | _ -> assert false (* typing error *)
    end
  | Sprint e -> 
    print_value Format.std_formatter (interpret_expr e);
    Format.fprintf Format.std_formatter "@."
and interpret_block b =
  List.iter interpret_stmt b

let run stmt_list =
  List.iter interpret_stmt stmt_list
