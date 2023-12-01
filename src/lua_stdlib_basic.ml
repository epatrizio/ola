open Ast

let rec tostring_value v =
  match v with
  | Vnil () -> "nil"
  | Vboolean b -> string_of_bool b
  | Vnumber (Ninteger i) -> string_of_int i
  | Vnumber (Nfloat f) -> string_of_float f
  | Vstring s -> s
  | Vtable (i, _) -> "table: " ^ string_of_int (Int32.to_int i)
  | Vfunction (i, _) | VfunctionStdLib (i, _) ->
    "function: " ^ string_of_int (Int32.to_int i)
  | VfunctionReturn vl -> (
    match vl with
    | [] -> ""
    | [ v ] -> tostring_value v
    | v :: tl -> tostring_value v ^ ", " ^ tostring_value (VfunctionReturn tl) )

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

let print v =
  List.iter
    (fun v -> Format.fprintf Format.std_formatter "%s@." (tostring_value v))
    v;
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
  match v with [ v ] -> [ Vstring (tostring_value v) ] | _ -> assert false
