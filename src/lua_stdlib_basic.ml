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

let typ v =
  match v with
  | [ Vnil () ] -> [ Vstring "nil" ]
  | [ Vboolean _ ] -> [ Vstring "boolean" ]
  | [ Vnumber _ ] -> [ Vstring "number" ]
  | [ Vtable _ ] -> [ Vstring "table" ]
  | [ Vfunction _ ] | [ VfunctionStdLib _ ] -> [ Vstring "function" ]
  | _ -> assert false

let tostring v =
  match v with
  | [ Vnil () ] -> [ Vstring "nil" ]
  | [ Vboolean true ] -> [ Vstring "true" ]
  | [ Vboolean false ] -> [ Vstring "false" ]
  | [ Vnumber (Ninteger i) ] -> [ Vstring (string_of_int i) ]
  | [ Vnumber (Nfloat f) ] -> [ Vstring (string_of_float f) ]
  | [ Vtable (i, _) ] ->
    [ Vstring ("table: " ^ string_of_int (Int32.to_int i)) ]
  | [ Vfunction (i, _) ] | [ VfunctionStdLib (i, _) ] ->
    [ Vstring ("function: " ^ string_of_int (Int32.to_int i)) ]
  | _ -> assert false
