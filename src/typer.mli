exception Typing_error of Ast.location option * string

val typecheck_function :
  Ast.Value.t -> (Ast.typ, Ast.location option * string) result

val typecheck_variadic :
     Ast.Value.t
  -> Ast.Value.t Env.t
  -> (Ast.typ, Ast.location option * string) result

val typecheck_var :
     ?strict:bool
  -> Ast.Value.var
  -> Ast.Value.t Env.t
  -> (Ast.typ, Ast.location option * string) result

val typecheck_expr :
     Ast.Value.expr
  -> Ast.Value.t Env.t
  -> (Ast.typ, Ast.location option * string) result

val typecheck_for_ctrl_expr :
     Ast.Value.expr
  -> Ast.Value.t Env.t
  -> (Ast.typ, Ast.location option * string) result

val typecheck_iterator_ctrl_el :
     Ast.Value.expr list
  -> Ast.Value.t Env.t
  -> (Ast.typ list, Ast.location option * string) result
