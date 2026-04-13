open Ast.Value

let loaded v env =
  match v with
  | [ Vstring modul ] ->
    let modul = Format.sprintf "%s.lua" modul in
    let is_loaded = Env.is_package_loaded modul env in
    ([ Vboolean is_loaded ], env)
  | _ -> assert false
