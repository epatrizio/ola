open Ast
open Ast.Value

let () = Random.self_init ()

let rec tostring_value v env =
  match v with
  | Vnil () -> ("nil", env)
  | Vboolean b -> (string_of_bool b, env)
  | Vnumber (Ninteger i) -> (string_of_int i, env)
  | Vnumber (Nfloat f) -> (string_of_float f, env)
  | Vstring s -> (s, env)
  | Vtable (i, tbl) as t -> begin
    match LuaTable.get_metatable tbl with
    | Some mt -> begin
      match LuaTable.get (Vstring "__tostring") mt with
      | Some (Vfunction (_, _, _) as f) ->
        let _, v2, env =
          match
            Interpret.interpret_fct f [ (Ast.empty_location (), Evalue t) ] env
          with
          | Error (_, msg) -> Lua_stdlib_common.error msg
          | Ok v -> v
        in
        tostring_value v2 env
      | Some _ ->
        Lua_stdlib_common.typing_error
          "metatable.__tostring: attempt to call a non function value"
      | None -> (Format.sprintf "table: %i" (Int32.to_int i), env)
    end
    | None -> (Format.sprintf "table: %i" (Int32.to_int i), env)
  end
  | Vfunction (i, _, _) | VfunctionStdLib (i, _) ->
    (Format.sprintf "function: %i" (Int32.to_int i), env)
  | VfunctionReturn vl | Vvariadic vl -> (
    match vl with
    | [] -> ("", env)
    | [ v ] -> tostring_value v env
    | v :: tl ->
      let s1, env = tostring_value v env in
      let s2, env = tostring_value (VfunctionReturn tl) env in
      (Format.sprintf "%s, %s" s1 s2, env) )

let asert v env =
  begin match v with
  | Vnil () :: [ Vstring msg ] | Vboolean false :: [ Vstring msg ] ->
    assert (
      print_endline (Format.sprintf "assert: %s" msg);
      false )
  | [ Vnil () ] | [ Vboolean false ] -> assert false
  | _ -> assert true
  end;
  ([ Vnil () ], env)

let next v env =
  try
    match v with
    | [ Vtable (_, tbl) ] | [ Vtable (_, tbl); Vnil () ] -> begin
      match LuaTable.next None tbl with
      | Some (LuaTable.Ikey i, v) -> ([ Vnumber (Ninteger i); v ], env)
      | Some (LuaTable.Kkey k, v) -> ([ k; v ], env)
      | None -> ([ Vnil () ], env)
    end
    | [ Vtable (_, tbl); v ] -> begin
      match LuaTable.next (Some v) tbl with
      | Some (LuaTable.Ikey i, v) -> ([ Vnumber (Ninteger i); v ], env)
      | Some (LuaTable.Kkey k, v) -> ([ k; v ], env)
      | None -> ([ Vnil () ], env)
    end
    | _ -> assert false
  with LuaTable.Table_error msg -> Lua_stdlib_common.error msg

let pairs v env =
  match v with
  | [ Vtable (i, tbl) ] ->
    ([ VfunctionStdLib (Random.bits32 (), next); Vtable (i, tbl); Vnil () ], env)
  | _ ->
    Lua_stdlib_common.typing_error
      "bad argument #1 to 'for iterator' (table expected)"

let inext v env =
  match v with
  | [ Vtable (_, tbl) ] | [ Vtable (_, tbl); Vnil () ] -> begin
    match LuaTable.inext (fun v -> v = Vnil ()) 0 tbl with
    | Some (i, v) -> ([ Vnumber (Ninteger i); v ], env)
    | None -> ([ Vnil () ], env)
  end
  | [ Vtable (_, tbl); Vnumber (Ninteger i) ] when i > 0 -> begin
    match LuaTable.inext (fun v -> v = Vnil ()) i tbl with
    | Some (i, v) -> ([ Vnumber (Ninteger i); v ], env)
    | None -> ([ Vnil () ], env)
  end
  | _ -> assert false

let ipairs v env =
  match v with
  | [ Vtable (i, tbl) ] ->
    ( [ VfunctionStdLib (Random.bits32 (), inext); Vtable (i, tbl); Vnil () ]
    , env )
  | _ ->
    Lua_stdlib_common.typing_error
      "bad argument #1 to 'for iterator' (table expected)"

let print v env =
  let sl =
    List.map
      (fun v ->
        let s, _env = tostring_value v env in
        s )
      v
  in
  Format.pp_print_list ~pp_sep Format.pp_print_string Format.std_formatter sl;
  Format.fprintf Format.std_formatter "@.";
  ([ Vnil () ], env)

let typ v env =
  match v with
  | [ Vnil () ] -> ([ Vstring "nil" ], env)
  | [ Vboolean _ ] -> ([ Vstring "boolean" ], env)
  | [ Vnumber _ ] -> ([ Vstring "number" ], env)
  | [ Vstring _ ] -> ([ Vstring "string" ], env)
  | [ Vtable _ ] -> ([ Vstring "table" ], env)
  | [ Vfunction _ ] | [ VfunctionStdLib _ ] -> ([ Vstring "function" ], env)
  | [ v ] ->
    Ast.print_value Format.err_formatter v;
    assert false
  | _ -> assert false

let tostring v env =
  match v with
  | [ v ] ->
    let s, env = tostring_value v env in
    ([ Vstring s ], env)
  | _ -> assert false

let getmetatable v env =
  match v with
  | [ Vtable (_, tbl) ] -> begin
    match LuaTable.get_metatable tbl with
    | Some mt -> begin
      match LuaTable.get (Vstring "__metatable") mt with
      | Some v -> ([ v ], env)
      | None -> ([ Vtable (Random.bits32 (), mt) ], env)
    end
    | None -> ([ Vnil () ], env)
  end
  | _ -> ([ Vnil () ], env)

(* TODO: memo, env must be updated. For now, it's impossible! *)
let setmetatable v env =
  match v with
  | Vtable (id, tbl) :: Vnil () :: _tl ->
    let tbl = LuaTable.remove_metatable tbl in
    ([ Vtable (id, tbl) ], env)
  | Vtable (id, tbl) :: Vtable (_, meta_tbl) :: _tl -> begin
    match LuaTable.get_metatable tbl with
    | Some mt -> begin
      match LuaTable.get (Vstring "__metatable") mt with
      | Some _ -> Lua_stdlib_common.error "cannot change a protected metatable"
      | None ->
        let tbl = LuaTable.set_metatable meta_tbl tbl in
        ([ Vtable (id, tbl) ], env)
    end
    | None ->
      let tbl = LuaTable.set_metatable meta_tbl tbl in
      ([ Vtable (id, tbl) ], env)
  end
  | Vtable (_, _) :: _tl ->
    Lua_stdlib_common.typing_error
      "bad argument #2 to 'setmetatable' (nil or table expected)"
  | _ :: _tl ->
    Lua_stdlib_common.typing_error
      "bad argument #1 to 'setmetatable' (nil or table expected)"
  | [] ->
    Lua_stdlib_common.typing_error
      "bad argument #1 to 'setmetatable' (nil or table expected)"
