(* Environment *)

let mk_fresh_name = Utils.str_counter_from "v" 1

module Names = Map.Make (String)
module Values = Map.Make (String)

type t =
  { names : Ast.name Names.t
  ; values : Ast.value Values.t
  ; scope : (Ast.name * Ast.name) list
  }

let empty () = { names = Names.empty; values = Values.empty; scope = [] }

let add_name n env =
  let fresh_name = mk_fresh_name () in
  let env_n = Names.add fresh_name n env.names in
  let env_v = Values.add fresh_name (Ast.Vnil ()) env.values in
  let env_s = (n, fresh_name) :: env.scope in
  (fresh_name, { names = env_n; values = env_v; scope = env_s })

let get_name n env =
  let rec get_n n scope =
    match scope with
    | [] -> add_name n env
    | (n_init, n_fresh) :: _tl when n_init = n -> (n_fresh, env)
    | _ :: tl -> get_n n tl
  in
  get_n n env.scope

let get_value n env =
  match Values.find_opt n env.values with Some v -> v | None -> assert false

let set_value n v env =
  match Values.find_opt n env.values with
  | Some _ ->
    let env_v = Values.add n v env.values in
    { names = env.names; values = env_v; scope = env.scope }
  | None -> assert false
