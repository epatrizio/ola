exception Evaluation_error of Ast.location option * string

module Eval_utils : sig
  val number_of_string : Ast.location option -> Ast.Value.t -> Ast.Value.t

  val integer_of_float_value : Ast.Value.t -> Ast.Value.t
end

val eval_unop :
     Ast.unop
  -> Ast.location * Ast.Value.t
  -> Ast.Value.t Env.t
  -> (Ast.Value.t * Ast.Value.t Env.t, Ast.location option * string) result

val eval_binop :
     Ast.binop
  -> Ast.location * Ast.Value.t
  -> Ast.location * Ast.Value.t
  -> Ast.Value.t Env.t
  -> (Ast.Value.t * Ast.Value.t Env.t, Ast.location option * string) result

val eval_bbinop : Ast.binop -> Ast.Value.t -> Ast.Value.t option
