(* Environment *)

let mk_fresh_name = Utils.str_counter_from "v" 1

module Names = Map.Make (String)
module Values = Map.Make (String)

type t =
  { names : Ast.name Names.t
  ; values : Ast.value Values.t
  ; globals : Ast.name Values.t
  ; locals : Ast.name Values.t list
  }

let empty () =
  { names = Names.empty
  ; values = Values.empty
  ; globals = Names.empty
  ; locals = []
  }

let scope_open env =
  { names = env.names
  ; values = env.values
  ; globals = env.globals
  ; locals = Names.empty :: env.locals
  }

let scope_close env =
  { names = env.names
  ; values = env.values
  ; globals = env.globals
  ; locals = List.tl env.locals
  }

let add_global n env =
  match Names.find_opt n env.globals with
  | Some fn -> (fn, env)
  | None ->
    let fresh_name = mk_fresh_name () in
    let env_n = Names.add fresh_name n env.names in
    let env_v = Values.add fresh_name (Ast.Vnil ()) env.values in
    let env_g = Names.add n fresh_name env.globals in
    ( fresh_name
    , { names = env_n; values = env_v; globals = env_g; locals = env.locals } )

let add_local n env =
  (* in current scope *)
  match env.locals with
  | [] -> assert false
  | names :: tl -> (
    match Names.find_opt n names with
    | Some fn -> (fn, env)
    | None ->
      let fresh_name = mk_fresh_name () in
      let env_n = Names.add fresh_name n env.names in
      let env_v = Values.add fresh_name (Ast.Vnil ()) env.values in
      let env_l = Names.add n fresh_name names in
      ( fresh_name
      , { names = env_n
        ; values = env_v
        ; globals = env.globals
        ; locals = env_l :: tl
        } ) )

let add_name n env =
  let rec add_n n locals =
    match locals with
    | [] -> None
    | names :: tl -> (
      match Names.find_opt n names with
      | None -> add_n n tl
      | Some fn -> Some (fn, env) )
  in
  match add_n n env.locals with
  | None -> add_global n env
  | Some (fn, env) -> (fn, env)

let get_name n env =
  let rec get_local_name n scopes =
    match scopes with
    | [] -> None
    | locals :: tl -> (
      match Names.find_opt n locals with
      | Some fn -> Some fn
      | None -> get_local_name n tl )
  in
  match get_local_name n env.locals with
  | Some fn -> (fn, env)
  | None -> (
    match Names.find_opt n env.globals with
    | Some fn -> (fn, env)
    | None -> add_global n env )

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
