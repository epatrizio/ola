open Ast

let flush _v =
  flush_all ();
  [ Vnil () ]

let write v =
  let sl = List.map (fun v -> Lua_stdlib_basic.tostring_value v) v in
  List.iter (fun s -> print_string s) sl;
  [ Vnil () ]
