(* Environment *)

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
