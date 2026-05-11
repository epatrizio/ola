(* *)

open Format
open Ola
open Ola.Ast
open Syntax

let () =
  print_endline "Hello, ocaml!";
  let env = Env.empty () in
  match
    let* env = Lua_stdlib.load env in
    let* vl, _env = Interpreter.process "init.lua" false env in
    (* let* vl, _env = Interpreter.process "examples/ocaml/init.lua" false env in *)
    (* let* data = Env.get_value "v0" env in *)
    begin match vl with
    | Vtable tbl :: _ ->
      begin match LuaTable.get (Vstring "__version") tbl with
      | Ok v | Error v ->
        print_value Format.std_formatter v;
        print_newline ()
      end;
      begin match LuaTable.get (Vstring "hello") tbl with
      | Ok (Vfunction (_, _, _) as fct) ->
        let _ =
          Interpret.interpret_fct fct
            [ (Ast.empty_location (), Evalue (Vstring "lua")) ]
            env
        in
        ()
      | Ok _ -> assert false
      | Error _v -> assert false (* TODO *)
      end;
      (* begin match LuaTable.get (Vstring "data") tbl with
      | Ok v | Error v -> print_value Format.std_formatter v; print_newline ()  -- table: ID
      end; *)
      Ok ()
    | _ -> assert false
    end
  with
  | Error (None, message) -> eprintf "%s@." message
  | Error (Some loc, message) -> eprintf "%a: %s@." Ast.pp_loc loc message
  | Ok _ -> ()
