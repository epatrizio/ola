(* Environment *)

module SMap = Map.Make (String)

type locals = string SMap.t

type 'a t =
  { values : 'a ref SMap.t
  ; globals : string SMap.t
  ; locals : locals
  }

let fresh =
  let count = ref ~-1 in
  fun () ->
    incr count;
    Format.sprintf "v%d" !count

let add_global n default_value env =
  let fresh_name = fresh () in
  let values = SMap.add fresh_name default_value env.values in
  let globals = SMap.add n fresh_name env.globals in
  (fresh_name, { env with values; globals })

let add_global_force n v env =
  let values = SMap.add n v env.values in
  let globals = SMap.add n n env.globals in
  { env with values; globals }

let add_local n default_value env =
  let fresh_name = fresh () in
  let values = SMap.add fresh_name default_value env.values in
  let locals = SMap.add n fresh_name env.locals in
  (fresh_name, { env with values; locals })

let get_name n default_value env =
  match SMap.find_opt n env.locals with
  | Some n -> (n, env)
  | None -> (
    match SMap.find_opt n env.globals with
    | Some n -> (n, env)
    | None -> add_global n default_value env )

let get_value n env =
  match SMap.find_opt n env.values with None -> assert false | Some v -> !v

let update_value n v env =
  match SMap.find_opt n env.values with
  | None -> assert false
  | Some value -> value := !v

let add_value n v env =
  match SMap.find_opt n env.values with
  | None -> assert false
  | Some _ ->
    let values = SMap.add n v env.values in
    { env with values }

let get_locals env = env.locals

let with_locals env locals = { env with locals }

let empty () =
  let values = SMap.empty in
  let globals = SMap.empty in
  let locals = SMap.empty in
  { values; globals; locals }
