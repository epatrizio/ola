open Ast

let interpret_expr expr =
  match expr with
  | Evalue (Vnumber (Ninteger i)) -> i
  | _ -> assert false

let interpret_stmt stmt =
  match stmt with
  | Sprint e -> print_int (interpret_expr e)
  | _ -> assert false

let run stmt =
  interpret_stmt stmt
