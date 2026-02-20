type block_pointer =
  | Begin
  | Last
  | Label of string

exception Interpretation_error of Ast.location option * string

val interpret_fct :
     Ast.Value.t
  -> Ast.Value.expr list
  -> Ast.Value.t Env.t
  -> ( Ast.Value.t * Ast.Value.t * Ast.Value.t Env.t
     , Ast.location option * string )
     result

val run :
     ?pt:block_pointer
  -> Ast.Value.block
  -> Ast.Value.t Env.t
  -> (Ast.Value.t Env.t, Ast.location option * string) result
