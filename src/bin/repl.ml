open Ola
open Syntax

let prompt () =
  try
    print_string "> ";
    read_line ()
  with End_of_file ->
    print_endline "Good bye, see you soon!";
    exit 0

let rec loop (chunk : Ast.block) (env : Ast.value Env.t) =
  let user_in = prompt () in
  let lexbuf = Sedlexing.Latin1.from_string user_in in
  try
    let lexer = Sedlexing.with_tokenizer Lexer.token lexbuf in
    let parser =
      MenhirLib.Convert.Simplified.traditional2revised Parser.chunk
    in
    let stmt = parser lexer in
    let stmt, env = Scope.analysis stmt env in
    let chunk = chunk @ stmt in
    let* env = Interpret.run ~pt:Interpret.Last chunk env in
    loop chunk env
  with
  | Lexer.Lexing_error message ->
    print_endline ("Lexical error: " ^ message);
    loop chunk env
  | Parser.Error ->
    print_endline "Syntax error";
    loop chunk env
  | Typer.Typing_error (_loc, message) ->
    print_endline ("Typing error: " ^ message);
    loop chunk env
  | Interpret.Interpretation_error (_loc, message) ->
    print_endline ("Interpretation error: " ^ message);
    loop chunk env

let run () =
  print_endline "ola, Lua language interpreter!";
  let env = Env.empty () in
  match Lua_stdlib.load env with
  | Ok env -> loop [] env
  | Error message ->
    print_endline ("Lua_stdlib.load error: " ^ message);
    exit 1
