exception Typing_error of Ast.location option * string

val typecheck_function :
  Ast.value -> (Ast.typ, Ast.location option * string) result

val typecheck_variadic :
  Ast.value -> (Ast.typ, Ast.location option * string) result

val typecheck_var :
     ?strict:bool
  -> Ast.var
  -> Ast.value Env.t
  -> (Ast.typ, Ast.location option * string) result

val typecheck_expr :
  Ast.expr -> Ast.value Env.t -> (Ast.typ, Ast.location option * string) result

val typecheck_for_ctrl_expr :
  Ast.expr -> Ast.value Env.t -> (Ast.typ, Ast.location option * string) result

val typecheck_iterator_ctrl_el :
     Ast.expr list
  -> Ast.value Env.t
  -> (Ast.typ list, Ast.location option * string) result
