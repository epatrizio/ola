(* Environment *)

let mk_fresh_name = Utils.str_counter_from "v" 1

let mk_fresh_funcname = Utils.str_counter_from "func" 1

module Names = Map.Make (String)
module Values = Map.Make (String)

type t =
  { names : Ast.name Names.t
  ; values : Ast.value Values.t
  ; globals : Ast.name Names.t
  ; locals : Ast.name Names.t
  }

let empty () =
  { names = Names.empty
  ; values = Values.empty
  ; globals = Values.empty
  ; locals = Values.empty
  }

let add_global mk n env =
  let fresh_name = mk () in
  let env_n = Names.add fresh_name n env.names in
  let env_v = Values.add fresh_name (Ast.Vnil ()) env.values in
  let env_g = Names.add n fresh_name env.globals in
  ( fresh_name
  , { names = env_n; values = env_v; globals = env_g; locals = env.locals } )

let add_local mk n env =
  let fresh_name = mk () in
  let env_n = Names.add fresh_name n env.names in
  let env_v = Values.add fresh_name (Ast.Vnil ()) env.values in
  let env_l = Names.add n fresh_name env.locals in
  ( fresh_name
  , { names = env_n; values = env_v; globals = env.globals; locals = env_l } )

let add_global_name = add_global mk_fresh_name

let add_global_funcname = add_global mk_fresh_funcname

let add_local_name = add_local mk_fresh_name

let add_local_funcname = add_local mk_fresh_funcname

let get_vname mk n env =
  match Values.find_opt n env.locals with
  | Some n -> (n, env)
  | None -> (
    match Values.find_opt n env.globals with
    | Some n -> (n, env)
    | None -> add_global mk n env )

let get_name = get_vname mk_fresh_name

let get_funcname = get_vname mk_fresh_funcname

let get_value n env =
  match Values.find_opt n env.values with Some v -> v | None -> assert false

let set_value n v env =
  match Values.find_opt n env.values with
  | Some _ ->
    let env_v = Values.add n v env.values in
    { names = env.names
    ; values = env_v
    ; globals = env.globals
    ; locals = env.locals
    }
  | None -> assert false
