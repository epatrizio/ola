open Ast

let execute v =
  match v with
  | [ Vstring cmd ] ->
    let i = Sys.command cmd in
    [ Vnumber (Ninteger i) ]
  | _ -> assert false
