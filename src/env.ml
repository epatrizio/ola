(* Environment *)

let () = Random.self_init ()

module SMap = Map.Make (String)

type locals = string SMap.t

type t =
  { values : Ast.value SMap.t
  ; globals : string SMap.t
  ; locals : locals
  }

let empty = { values = SMap.empty; globals = SMap.empty; locals = SMap.empty }

let fresh =
  let count = ref ~-1 in
  fun () ->
    incr count;
    Format.sprintf "v%d" !count

let add_global n env =
  let fresh_name = fresh () in
  let values = SMap.add fresh_name (Ast.Vnil ()) env.values in
  let globals = SMap.add n fresh_name env.globals in
  (fresh_name, { env with values; globals })

let add_local n env =
  let fresh_name = fresh () in
  let values = SMap.add fresh_name (Ast.Vnil ()) env.values in
  let locals = SMap.add n fresh_name env.locals in
  (fresh_name, { env with values; locals })

let get_name n env =
  match SMap.find_opt n env.locals with
  | Some n -> (n, env)
  | None -> (
    match SMap.find_opt n env.globals with
    | Some n -> (n, env)
    | None -> add_global n env )

let get_value n env =
  match SMap.find_opt n env.values with None -> assert false | Some v -> v

let set_value n v env =
  match SMap.find_opt n env.values with
  | None -> assert false
  | Some _ ->
    let values = SMap.add n v env.values in
    { env with values }

let get_locals env = env.locals

let with_locals env locals = { env with locals }

let stdlib_load env =
  let add_empty_lib name env =
    let values =
      SMap.add name (Ast.Vtable (Random.bits32 (), Table.empty)) env.values
    in
    let globals = SMap.add name name env.globals in
    { env with values; globals }
  in
  let add_function_lib lib_name fct_name fct env =
    let v = get_value lib_name env in
    match v with
    | Vtable (i, tbl) ->
      let tbl =
        Table.add (Ast.Vstring fct_name)
          (Ast.VfunctionStdLib (Random.bits32 (), fct))
          tbl
      in
      set_value lib_name (Vtable (i, tbl)) env
    | _ -> assert false
  in
  let env = add_empty_lib "basic" env in
  let env = add_empty_lib "math" env in
  let env = add_function_lib "math" "abs" Lua_stdlib_math.abs env in
  env
