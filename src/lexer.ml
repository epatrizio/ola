(* Lexical analyzer *)

open Parser

exception Lexing_error of string
let error message = raise (Lexing_error message)

let digit = [%sedlex.regexp? '0'..'9']
let letter = [%sedlex.regexp? 'a'..'z' | 'A'..'Z']
let number = [%sedlex.regexp? Plus digit]
let blank = [%sedlex.regexp? ' ' | '\t']
let newline = [%sedlex.regexp? '\r' | '\n' | "\r\n"]

let rec token buf =
  match%sedlex buf with
  | Plus (Chars " \t") -> token buf
  | number -> VALUE (Vnumber (Ninteger (int_of_string (Sedlexing.Latin1.lexeme buf))))
  (* | ',' -> COMMA *)
  | ';' -> SEMICOLON
  (* | '+' -> PLUS
  | '-' -> MINUS
  | '*' -> MUL
  | '/' -> DIV *)
  | '(' -> LPAREN
  | ')' -> RPAREN
  | "begin" -> BEGIN
  | "end" -> END
  | "print" -> PRINT
  | eof -> EOF
  | _ -> error "Unexpected character"
