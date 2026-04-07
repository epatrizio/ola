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

(* OLA entry point : Lua language interpreter *)
let () =
  Arg.parse options set_file usage;
  match
    if !in_file_name = "" then Repl.run ()
    else
      let env = Env.empty () in
      let* env = Lua_stdlib.load env in
      Interpreter.process !in_file_name !debug env
  with
  | Ok () -> ()
  | Error (None, message) -> eprintf "%s@." message
  | Error (Some loc, message) -> eprintf "%a: %s@." Ast.pp_loc loc message
