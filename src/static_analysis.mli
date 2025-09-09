exception Static_analysis_error of Ast.location option * string

module Variadic_func : sig
  val analyze : Ast.block -> (unit, 'a) result
end

module Const_var : sig
  val analyze : Ast.block -> (unit, 'a) result
end
