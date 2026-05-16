(* *)

open Format
open Ola
open Ola.Ast
open Syntax

(* WIP: LuaTable helpers (create a specific module ?) *)
let str_of_str_key key tbl =
  match LuaTable.get (Vstring key) tbl with
  | Ok (Vstring str) -> str
  (* | Error _ TODO: check LuaTable.get impl > metatable_field "__index" *)
  | _ -> assert false (* TODO: error ctrl *)

(* let int_of_str_key key tbl =
  match LuaTable.get (Vstring key) tbl with
  | Ok (Vnumber (Ninteger i)) -> i
  | _ -> assert false *)

let tbl_of_str_key key tbl =
  match LuaTable.get (Vstring key) tbl with
  | Ok (Vtable tbl) -> tbl
  | _ -> assert false

let int_of_int_key key tbl =
  match LuaTable.get (Vnumber (Ninteger key)) tbl with
  | Ok (Vnumber (Ninteger i)) -> i
  | _ -> assert false

(* **** *)

let () =
  print_endline "Hello, ocaml!";
  let env = Env.empty () in
  match
    let* env = Lua_stdlib.load env in
    let* vl, _env = Interpreter.process "init.lua" false env in
    (* let* vl, _env = Interpreter.process "examples/ocaml/init.lua" false env in *)

    (* let* data = Env.get_value "v0" env in *)
    (* WARNING! Limitation: we only have var names after scoping *)

    begin match vl with
    | Vtable tbl :: _ ->
      let version = str_of_str_key "__version" tbl in
      print_endline version;
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
      let tbl = tbl_of_str_key "data" tbl in
      let i1 = int_of_int_key 1 tbl in
      print_int i1;
      print_newline ();
      Ok ()
    | _ -> assert false
    end
  with
  | Error (None, message) -> eprintf "%s@." message
  | Error (Some loc, message) -> eprintf "%a: %s@." Ast.pp_loc loc message
  | Ok _ -> ()
