open Format
open Ola

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
      Ast.print_chunk Format.std_formatter chunk
    end;
    print_endline "interprete ...";
    let env = Env.empty () in
    let chunk, env = Scope.analysis chunk env in
    if debug then begin
      print_endline "debug mode: source after scope analysis view ...";
      Ast.print_chunk Format.std_formatter chunk
    end;
    let _ = Interpret.run chunk env in
    ();
    close_in ic
  with
  | Lexer.Lexing_error message ->
    eprintf "Lexical error: %s@." message;
    exit 1
  | Parser.Error ->
    let loc = Sedlexing.lexing_positions lexbuf in
    eprintf "Syntax error: %a@." Utils.location_info loc;
    exit 1
  | Interpret.Interpretation_error (loc, message) -> (
    match loc with
    | Some loc ->
      eprintf "Interpretation error: %a: %s@." Utils.location_info loc message
    | None ->
      eprintf "Interpretation error: %s@." message;
      exit 1 )

(* OLA entry point : Lua language interpreter *)
let () =
  Arg.parse options set_file usage;
  if !in_file_name = "" then Repl.run () else process !in_file_name !debug
