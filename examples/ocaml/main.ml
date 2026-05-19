(* *)

open Format
open Ola
open Ola.Ast
open Syntax

(* WIP: LuaTable helpers (create a specific module ?) *)

let val_of_val_key key tbl =
  match LuaTable.get key tbl with
  | Ok v -> Ok v
  | Error (Vnil ()) -> Error (None, "attempt to access a non-existent table key")
  | Error v -> Ok v (* metatable_field "__index" *)

let str_of_str_key key tbl =
  match val_of_val_key (Vstring key) tbl with
  | Ok (Vstring str) -> Ok str
  | Ok _ -> Error (None, "wrong value type, string expected")
  | Error (l, msg) -> Error (l, msg ^ key)

(* let int_of_str_key key tbl =
  match LuaTable.get (Vstring key) tbl with
  | Ok (Vnumber (Ninteger i)) -> i
  | _ -> assert false *)

let tbl_of_str_key key tbl =
  match val_of_val_key (Vstring key) tbl with
  | Ok (Vtable tbl) -> Ok tbl
  | Ok _ -> Error (None, "wrong value type, table expected")
  | Error (l, msg) -> Error (l, msg ^ key)

let int_of_int_key key tbl =
  match val_of_val_key (Vnumber (Ninteger key)) tbl with
  | Ok (Vnumber (Ninteger i)) -> Ok i
  | Ok _ -> Error (None, "wrong value type, int expected")
  | Error (l, msg) -> Error (l, msg ^ string_of_int key)

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
      let* version = str_of_str_key "__version" tbl in
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
      let* tbl = tbl_of_str_key "data" tbl in
      let* i1 = int_of_int_key 1 tbl in
      print_int i1;
      print_newline ();
      Ok ()
    | _ -> assert false
    end
  with
  | Error (None, message) -> eprintf "%s@." message
  | Error (Some loc, message) -> eprintf "%a: %s@." Ast.pp_loc loc message
  | Ok _ -> ()
