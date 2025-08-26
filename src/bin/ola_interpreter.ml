open Format
open Ola
open Syntax

let debug = ref false

let in_file_name = ref ""

let set_file s = in_file_name := s

let options = [ ("--debug", Arg.Set debug, " Debug mode") ]

let usage =
  "usage: dune exec ./src/bin/ola_interpreter.exe ./test/file_name.lua \
   [options]"

let process source_code_file debug =
  let ic = open_in source_code_file in
  let lexbuf = Sedlexing.Utf8.from_channel ic in
  try
    Sedlexing.set_filename lexbuf source_code_file;
    let lexer = Sedlexing.with_tokenizer Lexer.token lexbuf in
    let parser =
      MenhirLib.Convert.Simplified.traditional2revised Parser.chunk
    in
    let chunk = parser lexer in
    if debug then begin
      print_endline "debug mode: initial source view ...";
      Ast.print_block Format.std_formatter chunk
    end;
    print_endline "interprete ...";
    let env = Env.empty () in
    let env = Lua_stdlib.load env in
    let chunk, env = Scope.analysis chunk env in
    if debug then begin
      print_endline "debug mode: source after scope analysis view ...";
      Ast.print_block Format.std_formatter chunk
    end;
    let* _env = Interpret.run chunk env in
    Ok (close_in ic)
  with
  | Lexer.Lexing_error message ->
    let message = sprintf "Lexical error: %s" message in
    Error (None, message)
  | Parser.Error ->
    let loc = Sedlexing.lexing_positions lexbuf in
    Error (Some loc, "Syntax error")
  | Interpret.Interpretation_error (loc, message) ->
    let message = sprintf "Interpretation error: %s" message in
    Error (loc, message)

(* OLA entry point : Lua language interpreter *)
let () =
  Arg.parse options set_file usage;
  match
    if !in_file_name = "" then Repl.run () else process !in_file_name !debug
  with
  | Ok () -> ()
  | Error (None, message) -> eprintf "%s@." message
  | Error (Some loc, message) -> eprintf "%a: %s@." Ast.pp_loc loc message
