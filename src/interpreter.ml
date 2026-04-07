open Format

(* open Ola *)
open Syntax

(* TODO:
  print_endline ?
  try catch here or in bin.ola_interpreter ?
  coherence Repl ? *)

let process source_code_file debug env =
  let ic = open_in source_code_file in
  let lexbuf = Sedlexing.Utf8.from_channel ic in
  try
    Sedlexing.set_filename lexbuf source_code_file;
    let lexer = Sedlexing.with_tokenizer Lexer.token lexbuf in
    let parser =
      MenhirLib.Convert.Simplified.traditional2revised Parser.chunk
    in
    print_endline "lexing parsing ...";
    let chunk = parser lexer in
    if debug then begin
      print_endline "debug mode: initial source view ...";
      Ast.print_block Format.std_formatter chunk
    end;
    print_endline "static analyses ...";
    let* () = Static_analysis.Variadic_func.analyze chunk in
    let* () = Static_analysis.Const_var.analyze chunk in
    (* let env = Env.empty () in -- WIP *)
    print_endline "stdlib loading ...";
    (* let* env = Lua_stdlib.load env in -- WIP *)
    print_endline "scope analysis ...";
    let chunk, env = Scope.analysis chunk env in
    if debug then begin
      print_endline "debug mode: source after scope analysis view ...";
      Ast.print_block Format.std_formatter chunk
    end;
    print_endline "interprete ...";
    let* _env = Interpret.run chunk env in
    Ok (close_in ic)
  with
  | Lexer.Lexing_error message ->
    let message = sprintf "Lexical error: %s" message in
    Error (None, message)
  | Parser.Error ->
    let loc = Sedlexing.lexing_positions lexbuf in
    Error (Some loc, "Syntax error")
  | Env.Env_error (_, message) ->
    let message = sprintf "Env error: %s" message in
    Error (None, message)
  | Static_analysis.Static_analysis_error (loc, message) ->
    let message = sprintf "Static analysis error: %s" message in
    Error (loc, message)
  | Typer.Typing_error (loc, message) ->
    let message = sprintf "Typing error: %s" message in
    Error (loc, message)
  | Evaluator.Evaluation_error (loc, message) ->
    let message = sprintf "Evaluation error: %s" message in
    Error (loc, message)
  | Interpret.Interpretation_error (loc, message) ->
    let message = sprintf "Interpretation error: %s" message in
    Error (loc, message)
