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

let interpret_stmt stmt =
  match stmt with
  | Sblock _b -> assert false
  | Sprint e -> 
    print_value Format.std_formatter (interpret_expr e);
    Format.fprintf Format.std_formatter "@."

(* and interpret_block bl =
  match bl with
  | Bstmt [] -> ()
  | Bstmt [s] -> interpret_stmt s
  | Bstmt (s :: tl) -> interpret_stmt s; interpret_block (Bstmt tl) *)

let run stmt_list =
  List.iter interpret_stmt stmt_list
