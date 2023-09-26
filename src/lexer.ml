(* Lexical analyzer *)

open Parser

exception Lexing_error of string
let error message = raise (Lexing_error message)

let digit = [%sedlex.regexp? '0'..'9']
let letter = [%sedlex.regexp? 'a'..'z' | 'A'..'Z']
let hexdigit = [%sedlex.regexp? digit | 'a' .. 'f' | 'A' .. 'F']
let blank = [%sedlex.regexp? ' ' | '\t']
let newline = [%sedlex.regexp? '\r' | '\n' | "\r\n"]

let nil = [%sedlex.regexp? "nil"]
let boolean = [%sedlex.regexp? "true" | "false"]
let integer = [%sedlex.regexp? Plus digit]
let exp = [%sedlex.regexp? ('e' | 'E'), (Opt ('-' | '+')), (Plus digit)]
let float = [%sedlex.regexp? Opt (Plus digit), Opt '.', Opt (Plus digit), Opt exp]

let rec token buf =
  match%sedlex buf with
  | Plus (Chars " \t") -> token buf
  | newline -> token buf
  | integer -> VALUE (Vnumber (Ninteger (int_of_string (Sedlexing.Latin1.lexeme buf))))
  | float -> VALUE (Vnumber (Nfloat (float_of_string (Sedlexing.Latin1.lexeme buf))))
  (* | ',' -> COMMA *)
  | ';' -> SEMICOLON
  | '+' -> PLUS
  | '-' -> MINUS
  | '*' -> MUL
  | '<' -> LT
  | "<=" -> LE
  | '>' -> GT
  | ">=" -> GE
  | "==" -> EQ
  | "~=" -> NEQ
  (*| '/' -> DIV *)
  | '(' -> LPAREN
  | ')' -> RPAREN
  | nil -> VALUE (Vnil ())
  | boolean -> VALUE (Vboolean (bool_of_string (Sedlexing.Latin1.lexeme buf)))
  | "not" -> NOT
  | "and" -> AND
  | "or" -> OR
  | "while" -> WHILE
  | "do" -> DO
  | "end" -> END
  | "print" -> PRINT
  | "--" -> comment buf
  | "--[[" -> multiline_comment buf
  | eof -> EOF
  | _ -> error "Unexpected character"

and comment buf =
  match%sedlex buf with
  | newline -> token buf
  | any -> comment buf
  | _ -> assert false

and multiline_comment buf =
  match%sedlex buf with
  | "--]]" -> token buf
  | newline -> multiline_comment buf
  | eof -> error "Unterminated comment"
  | any -> multiline_comment buf
  | _ -> assert false
