open Ast

let flush _v env =
  flush_all ();
  ([ Vnil () ], env)

let write v env =
  let sl =
    List.map
      (fun v ->
        let s, _env = Lua_stdlib_basic.tostring_value v env in
        s )
      v
  in
  List.iter
    (fun s ->
      print_string s;
      Format.print_flush () )
    sl;
  ([ Vnil () ], env)
