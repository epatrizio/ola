(* Environment *)

module SMap = Map.Make (String)

type locals = string SMap.t

type 'a t =
  { values : 'a ref SMap.t
  ; globals : string SMap.t
  ; locals : locals
  }

(* hack: "unit option *" to get the right exception format (Ast.location option * string) *)
exception Env_error of unit option * string

let error message = raise (Env_error (None, message))

let fresh =
  let count = ref ~-1 in
  fun () ->
    incr count;
    Format.sprintf "v%d" !count

let add_global n default_value env =
  let default_value = ref default_value in
  let fresh_name = fresh () in
  let values = SMap.add fresh_name default_value env.values in
  let globals = SMap.add n fresh_name env.globals in
  (fresh_name, { env with values; globals })

let add_global_force n v env =
  let v = ref v in
  let values = SMap.add n v env.values in
  let globals = SMap.add n n env.globals in
  { env with values; globals }

let add_local n default_value env =
  let default_value = ref default_value in
  let fresh_name = fresh () in
  let values = SMap.add fresh_name default_value env.values in
  let locals = SMap.add n fresh_name env.locals in
  (fresh_name, { env with values; locals })

let add_local_force n v env =
  let v = ref v in
  let values = SMap.add n v env.values in
  let locals = SMap.add n n env.locals in
  { env with values; locals }

let get_name n default_value env =
  let default_value = ref default_value in
  match SMap.find_opt n env.locals with
  | Some n -> (n, env)
  | None -> (
    match SMap.find_opt n env.globals with
    | Some n -> (n, env)
    | None -> add_global n !default_value env )

let get_value n env =
  match SMap.find_opt n env.values with
  | None ->
    error (Format.sprintf "name: %s not found in get_value env.values" n)
  | Some v -> Ok !v

let update_value n v env =
  let v = ref v in
  match SMap.find_opt n env.values with
  | None ->
    error (Format.sprintf "name: %s not found in update_value env.values" n)
  | Some value ->
    value := !v;
    Ok ()

let add_value n v env =
  let v = ref v in
  match SMap.find_opt n env.values with
  | None ->
    error (Format.sprintf "name: %s not found in add_value env.values" n)
  | Some _ ->
    let values = SMap.add n v env.values in
    Ok { env with values }

let get_locals env = env.locals

let with_locals env locals = { env with locals }

let empty () =
  let values = SMap.empty in
  let globals = SMap.empty in
  let locals = SMap.empty in
  { values; globals; locals }
