open Format
open Ola

let debug = ref false

let in_file_name = ref ""
let set_file s = in_file_name := s

let options = [
  "--debug", Arg.Set debug, " Debug mode"
]

let usage = "usage: dune exec ./src/bin/ola_interpreter.exe [options] test/file_name.lua"

let process source_code_file _debug =
  try
    let ic = open_in source_code_file in
    let lexbuf = Sedlexing.Utf8.from_channel ic in
    Sedlexing.set_filename lexbuf source_code_file;
    let lexer = Sedlexing.with_tokenizer Lexer.token lexbuf in
    let parser = MenhirLib.Convert.Simplified.traditional2revised Parser.script in
    let ast = parser lexer in
      Interpret.run ast;
      close_in ic
  with
    | Lexer.Lexing_error message ->
        eprintf "Lexical error: %s@." message;
        exit 1
    | Parser.Error ->
        eprintf "Syntax error@.";
        exit 1
    | _ ->
        eprintf "Uncaught error@.";
        exit 1

(* OLA entry point : a Lua language interpreter *)
let _ =
  Arg.parse options set_file usage;
  if !in_file_name = "" then begin
    eprintf "init error: missing test file name to interpret!@.";
    Arg.usage options usage;
    exit 1
  end;
  process !in_file_name !debug
