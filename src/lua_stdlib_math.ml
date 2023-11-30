open Ast

let abs v =
  match v with
  | [ Vnumber (Ninteger i) ] -> [ Vnumber (Ninteger (abs i)) ]
  | [ Vnumber (Nfloat f) ] -> [ Vnumber (Nfloat (abs_float f)) ]
  | [ Vstring s ] ->
    let f = Lua_stdlib_common.float_of_string s in
    [ Vnumber (Nfloat (abs_float f)) ]
  | _ ->
    Lua_stdlib_common.typing_error
      "bad argument #1 to math.abs function (number expected)"

let trigo func v =
  match v with
  | [ Vnumber (Ninteger i) ] -> [ Vnumber (Nfloat (func (float_of_int i))) ]
  | [ Vnumber (Nfloat f) ] -> [ Vnumber (Nfloat (func f)) ]
  | [ Vstring s ] ->
    let f = Lua_stdlib_common.float_of_string s in
    [ Vnumber (Nfloat (func f)) ]
  | _ ->
    Lua_stdlib_common.typing_error
      "bad argument #1 to math.'trigonometry' function (number expected)"

let cos v = trigo Stdlib.cos v

let sin v = trigo Stdlib.sin v

let tan v = trigo Stdlib.tan v
