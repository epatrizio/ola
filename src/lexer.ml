(* Lexical analyzer *)

open Parser

exception Lexing_error of string

let error message = raise (Lexing_error message)

(* Taken from https://github.com/OCamlPro/owi/blob/main/src/lexer.ml *)
let mk_string _buf s =
  let b = Buffer.create (String.length s) in
  let i = ref 0 in
  while !i < String.length s do
    let c =
      if s.[!i] <> '\\' then s.[!i]
      else
        match
          incr i;
          s.[!i]
        with
        | 'n' -> '\n'
        | 'r' -> '\r'
        | 't' -> '\t'
        | '\\' -> '\\'
        | '\'' -> '\''
        | '\"' -> '\"'
        (* | 'u' ->
           let j = !i + 2 in
           i := String.index_from s j '}';
           let n = int_of_string ("0x" ^ String.sub s j (!i - j)) in
           let bs = Wutf8.encode [ n ] in
           Buffer.add_substring b bs 0 (String.length bs - 1);
           bs.[String.length bs - 1] *)
        | h ->
          incr i;
          if !i >= String.length s then error "illegal escape in string";
          let str = Format.sprintf "0x%c%c" h s.[!i] in
          begin
            match int_of_string_opt str with
            | None -> error "illegal escape in string"
            | Some n -> Char.chr n
          end
    in
    Buffer.add_char b c;
    incr i
  done;
  Buffer.contents b

let digit = [%sedlex.regexp? '0' .. '9']

let letter = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z']

let hexdigit = [%sedlex.regexp? digit | 'a' .. 'f' | 'A' .. 'F']

let blank = [%sedlex.regexp? ' ' | '\t']

let newline = [%sedlex.regexp? '\r' | '\n' | "\r\n"]

let nil = [%sedlex.regexp? "nil"]

let boolean = [%sedlex.regexp? "true" | "false"]

let integer = [%sedlex.regexp? Plus digit]

let exp = [%sedlex.regexp? ('e' | 'E'), Opt ('-' | '+'), Plus digit]

let float =
  [%sedlex.regexp? Opt (Plus digit), Opt '.', Opt (Plus digit), Opt exp]

let str_double_quotes =
  [%sedlex.regexp? "\"", Star (Sub (any, "\"") | "\\\""), "\""]

let str_single_quotes =
  [%sedlex.regexp? "'", Star (Sub (any, "\'") | "\\\'"), "'"]

let rec token buf =
  match%sedlex buf with
  | Plus (Chars " \t") -> token buf
  | newline -> token buf
  | nil -> VALUE (Vnil ())
  | boolean -> VALUE (Vboolean (bool_of_string (Sedlexing.Latin1.lexeme buf)))
  | integer ->
    VALUE (Vnumber (Ninteger (int_of_string (Sedlexing.Latin1.lexeme buf))))
  | float ->
    VALUE (Vnumber (Nfloat (float_of_string (Sedlexing.Latin1.lexeme buf))))
  | str_double_quotes | str_single_quotes ->
    let s = Sedlexing.Utf8.lexeme buf in
    let s = String.sub s 1 (String.length s - 2) in
    let s = mk_string buf s in
    VALUE (Vstring s)
  (* | ',' -> COMMA *)
  | ';' -> SEMICOLON
  | '+' -> PLUS
  | '-' -> MINUS
  | '*' -> MUL
  | '/' -> DIV
  | "//" -> FLDIV
  | '%' -> MOD
  | '^' -> EXP
  | '#' -> SHARP
  | '<' -> LT
  | "<=" -> LE
  | '>' -> GT
  | ">=" -> GE
  | "==" -> EQ
  | "~=" -> NEQ
  | ".." -> DDOT
  | '(' -> LPAREN
  | ')' -> RPAREN
  | "not" -> NOT
  | "and" -> AND
  | "or" -> OR
  | "while" -> WHILE
  | "do" -> DO
  | "end" -> END
  | "repeat" -> REPEAT
  | "until" -> UNTIL
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
