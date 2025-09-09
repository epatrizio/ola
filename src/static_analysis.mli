exception Static_analysis_error of Ast.location * string

module Variadic_func : sig
  val analyze : Ast.block -> (unit, 'a) result
end
