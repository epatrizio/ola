open Ast

let asert v =
  begin
    match v with
    | Vnil () :: [ Vstring msg ] | Vboolean false :: [ Vstring msg ] ->
      assert (
        print_endline ("assert: " ^ msg);
        false )
    | [ Vnil () ] | [ Vboolean false ] -> assert false
    | _ -> assert true
  end;
  [ Vnil () ]
