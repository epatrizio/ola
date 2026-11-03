(* *)

open Format
open Ola
open Ola.Ast
open Syntax

module Helper : sig
  val get_from_str_key :
    string -> LuaTable.t -> (Value.t, 'a option * string) result

  val get_function_from_str_key :
    string -> LuaTable.t -> (Value.t, 'a option * string) result

  val get_table_from_str_key :
    string -> LuaTable.t -> (LuaTable.t, 'a option * string) result

  val get_luatable : Value.t list -> (LuaTable.t, 'a option * string) result

  val print : Value.t -> unit
end = struct
  let get_from_str_key key tbl =
    match LuaTable.get_from_str_key key tbl with
    | Ok value -> Ok value
    | Error _ -> Error (None, "key does not exists in table: " ^ key)

  let get_function_from_str_key key tbl =
    match get_from_str_key key tbl with
    | Ok (Vfunction _ as fct) -> Ok fct
    | Ok _ -> Error (None, "wrong value type, function expected")
    | Error _ as err -> err

  let get_table_from_str_key key tbl =
    match get_from_str_key key tbl with
    | Ok (Vtable tbl) -> Ok tbl
    | Ok _ -> Error (None, "wrong value type, table expected")
    | Error _ as err -> err

  let get_luatable = function
    | Value.Vtable tbl :: _ -> Ok tbl
    | _ -> Error (None, "wrong value type, luatable expected")

  let print value =
    print_value Format.std_formatter value;
    print_newline ()
end

(* **** *)

open Helper

let env_init () =
  let env = Env.empty () in
  Lua_stdlib.load env

let () =
  print_endline "Hello, OCaml!";
  match
    let* env = env_init () in
    let* vl, _env = Interpreter.process "mod.lua" false env in

    let* tbl = get_luatable vl in
    let* version = get_from_str_key "__version" tbl in
    print version;

    let* fct = get_function_from_str_key "hello" tbl in
    let _ =
      Interpret.interpret_fct fct
        [ (Ast.empty_location (), Evalue (Vstring "Lua")) ]
        env
    in
    ();
    let* tbl = get_table_from_str_key "data" tbl in
    let* s1 = get_from_str_key "key_1" tbl in
    print s1;
    Ok ()
  with
  | Error (None, message) -> eprintf "%s@." message
  | Error (Some loc, message) -> eprintf "%a: %s@." Ast.pp_loc loc message
  | Ok _ -> ()
