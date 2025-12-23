open Ola
open Syntax

type prompt =
  | Phelp
  | Pstmt of string

let prompt () =
  try
    print_string "> ";
    match read_line () with
    | "help" -> Phelp
    | user_in ->
      if String.starts_with ~prefix:"expr:" user_in then
        let len = String.length user_in in
        let expr = String.sub user_in 5 (len - 5) in
        let expr = String.trim expr in
        Pstmt ("print(type(" ^ expr ^ "), " ^ expr ^ ")")
      else Pstmt user_in
  with End_of_file ->
    print_endline "Good bye, see you soon!";
    exit 0

let help () =
  print_endline "'<statement>' cmd: execute the statement";
  print_endline
    "'expr: <expression>' cmd: evaluate the expression and print (type, value)";
  print_endline "'help' cmd: print this message";
  print_endline "Ctrl+D: end of file, repl cleanly exit";
  print_endline "Ctrl+C: repl forced exit"

let rec loop (chunk : Ast.block) (env : Ast.value Env.t) =
  match prompt () with
  | Phelp ->
    help ();
    loop chunk env
  | Pstmt user_in -> (
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
      loop chunk env )

let run () =
  print_endline "ola, Ocaml LuA language interpreter!";
  print_endline "'help' command: print ola repl help";
  let env = Env.empty () in
  match Lua_stdlib.load env with
  | Ok env -> loop [] env
  | Error message ->
    print_endline ("Lua_stdlib.load error: " ^ message);
    exit 1
