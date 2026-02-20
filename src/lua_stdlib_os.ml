open Ast.Value

let execute v env =
  match v with
  | [ Vstring cmd ] ->
    let i = Sys.command cmd in
    ([ Vnumber (Ninteger i) ], env)
  | _ -> assert false
