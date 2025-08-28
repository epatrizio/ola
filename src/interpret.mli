type block_pointer =
  | Begin
  | Last
  | Label of string

exception Interpretation_error of Ast.location option * string

val run :
     ?pt:block_pointer
  -> Ast.block
  -> Ast.value Env.t
  -> (Ast.value Env.t, Ast.location option * string) result
