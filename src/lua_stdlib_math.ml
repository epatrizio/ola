open Ast

let abs v =
  match v with
  | [ Vnumber (Ninteger i) ] -> [ Vnumber (Ninteger (abs i)) ]
  | [ Vnumber (Nfloat f) ] -> [ Vnumber (Nfloat (abs_float f)) ]
  | [ Vstring s ] ->
    let f = Lua_stdlib_common.float_of_string s in
    [ Vnumber (Nfloat (abs_float f)) ]
  | _ -> assert false
