exception Lexing_error of string

val token : Sedlexing.lexbuf -> Parser.token
