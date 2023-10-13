(* Environment *)

let mk_fresh_name = Utils.str_counter_from "v" 1

module Names = Map.Make (String)
module Values = Map.Make (String)

type t =
  { names : Ast.name Names.t
  ; values : Ast.value Values.t
  }

let empty () = { names = Names.empty; values = Values.empty }

let add_name n env =
  match Names.find_opt n env.names with
  | Some fn -> (fn, env)
  | None ->
    let fresh_name = mk_fresh_name () in
    let env_n = Names.add n fresh_name env.names in
    let env_v = Values.add fresh_name (Ast.Vnil ()) env.values in
    (fresh_name, { names = env_n; values = env_v })

let get_value n env =
  match Values.find_opt n env.values with Some v -> v | None -> assert false

let set_value n v env =
  match Values.find_opt n env.values with
  | Some _ ->
    let env_v = Values.add n v env.values in
    { names = env.names; values = env_v }
  | None -> assert false
