open Ast

let rec interpret_expr expr =
  match expr with
  | Evalue (Vnumber (Ninteger i)) -> i
  | Evalue _ -> assert false
  | Eident _i -> assert false
  | Ebinop (Badd, e1, e2) -> interpret_expr e1 + interpret_expr e2
  | Ebinop (Bsub, e1, e2) -> interpret_expr e1 - interpret_expr e2
  | Ebinop (Bmul, e1, e2) -> interpret_expr e1 * interpret_expr e2

let interpret_stmt stmt =
  match stmt with
  | Sblock _b -> assert false
  | Sprint e -> print_int (interpret_expr e); print_newline ()

(* and interpret_block bl =
  match bl with
  | Bstmt [] -> ()
  | Bstmt [s] -> interpret_stmt s
  | Bstmt (s :: tl) -> interpret_stmt s; interpret_block (Bstmt tl) *)

let run stmt =
  interpret_stmt stmt
