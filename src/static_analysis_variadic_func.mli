exception Static_analysis_error of Ast.location * string

val analyze : Ast.block -> (unit, 'a) result
