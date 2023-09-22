open Ast

let rec interpret_expr expr =
  let interpret_binop_expr binop expr1 expr2 =
    let {typ = t1; value = v1} = interpret_expr expr1 in
    let {typ = t2; value = v2} = interpret_expr expr2 in begin
      match t1, v1, t2, v2 with
      | Tnumber Tinteger, Vnumber (Ninteger i1), Tnumber Tinteger, Vnumber (Ninteger i2) ->
        begin match binop with
          | Badd -> {typ = Tnumber Tinteger; value = Vnumber (Ninteger (i1 + i2))}
          | Bsub -> {typ = Tnumber Tinteger; value = Vnumber (Ninteger (i1 - i2))}
          | Bmul -> {typ = Tnumber Tinteger; value = Vnumber (Ninteger (i1 * i2))}
        end
      | _ -> assert false (* typing error *)
    end
  in
  match expr with
  | Evalue Vnil -> {typ = Tnil; value = Vnil}
  | Evalue (Vnumber (Ninteger i)) ->
    {typ = Tnumber Tinteger; value = Vnumber (Ninteger i)}
  | Evalue _ -> assert false
  | Eident _i -> assert false
  | Eunop (Uminus, e) ->
    let {typ = t; value = v} = interpret_expr e in begin
      match t, v with
      | Tnumber Tinteger, Vnumber (Ninteger i) ->
        {typ = Tnumber Tinteger; value = Vnumber (Ninteger (-i))}
      | _ -> assert false (* typing error *)
    end
  | Ebinop (Badd, e1, e2) -> interpret_binop_expr Badd e1 e2
  | Ebinop (Bsub, e1, e2) -> interpret_binop_expr Bsub e1 e2
  | Ebinop (Bmul, e1, e2) -> interpret_binop_expr Bmul e1 e2

let interpret_stmt stmt =
  match stmt with
  | Sblock _b -> assert false
  | Sprint e -> 
    print_typed_value Format.std_formatter (interpret_expr e);
    Format.fprintf Format.std_formatter "@."

(* and interpret_block bl =
  match bl with
  | Bstmt [] -> ()
  | Bstmt [s] -> interpret_stmt s
  | Bstmt (s :: tl) -> interpret_stmt s; interpret_block (Bstmt tl) *)

let run stmt_list =
  List.iter interpret_stmt stmt_list
