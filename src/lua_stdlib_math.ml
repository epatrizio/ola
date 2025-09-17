open Ast

let abs v env =
  match v with
  | [ Vnumber (Ninteger i) ] -> ([ Vnumber (Ninteger (abs i)) ], env)
  | [ Vnumber (Nfloat f) ] -> ([ Vnumber (Nfloat (abs_float f)) ], env)
  | [ Vstring s ] ->
    let f = Lua_stdlib_common.float_of_string s in
    ([ Vnumber (Nfloat (abs_float f)) ], env)
  | _ ->
    Lua_stdlib_common.typing_error
      "bad argument #1 to math.abs function (number expected)"

(* todo: specification not fully met *)
let random v env =
  match v with
  | [] -> ([ Vnumber (Nfloat (Random.float 1.)) ], env)
  | [ Vnumber (Ninteger 0) ] -> ([ Vnumber (Ninteger (Random.bits ())) ], env)
  | [ Vnumber (Ninteger i) ] -> ([ Vnumber (Ninteger (Random.int i)) ], env)
  | [ Vnumber (Ninteger _i); Vnumber (Ninteger j) ] ->
    ([ Vnumber (Ninteger (Random.int j)) ], env)
  | _ -> assert false

let trigo func v env =
  match v with
  | [ Vnumber (Ninteger i) ] ->
    ([ Vnumber (Nfloat (func (float_of_int i))) ], env)
  | [ Vnumber (Nfloat f) ] -> ([ Vnumber (Nfloat (func f)) ], env)
  | [ Vstring s ] ->
    let f = Lua_stdlib_common.float_of_string s in
    ([ Vnumber (Nfloat (func f)) ], env)
  | _ ->
    Lua_stdlib_common.typing_error
      "bad argument #1 to math.'trigonometry' function (number expected)"

let cos v env = trigo Stdlib.cos v env

let sin v env = trigo Stdlib.sin v env

let tan v env = trigo Stdlib.tan v env
