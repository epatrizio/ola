exception Typing_error of Ast.location option * string

val typecheck_var :
  Ast.var -> Ast.value Env.t -> (Ast.typ, Ast.location option * string) result

val typecheck_expr :
  Ast.expr -> Ast.value Env.t -> (Ast.typ, Ast.location option * string) result

val typecheck_functioncall :
     Ast.functioncall
  -> Ast.value Env.t
  -> (Ast.typ, Ast.location option * string) result

val typecheck_stmt :
  Ast.stmt -> Ast.value Env.t -> (unit, Ast.location option * string) result
