val process :
     string
  -> bool
  -> Ast.Value.t Env.t
  -> (Ast.Value.t list * Ast.Value.t Env.t, Ast.location option * string) result
