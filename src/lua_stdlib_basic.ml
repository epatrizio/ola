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

let next v =
  try
    match v with
    | [ Vtable (_, tbl) ] | [ Vtable (_, tbl); Vnil () ] -> begin
      match Table.next None tbl with
      | Some (Table.Ikey i, v) -> [ Vnumber (Ninteger i); v ]
      | Some (Table.Kkey k, v) -> [ k; v ]
      | None -> [ Vnil () ]
    end
    | [ Vtable (_, tbl); Vnumber (Ninteger i) ] -> begin
      match Table.next (Some i) tbl with
      | Some (Table.Ikey i, v) -> [ Vnumber (Ninteger i); v ]
      | Some (Table.Kkey k, v) -> [ k; v ]
      | None -> [ Vnil () ]
    end
    | [ Vtable _; _ ] -> Lua_stdlib_common.typing_error "invalid key to 'next'"
    | _ -> assert false
  with Table.Table_error msg -> Lua_stdlib_common.error msg

let print v =
  let s = List.map (fun v -> tostring_value v) v in
  Format.pp_print_list ~pp_sep Format.pp_print_string Format.std_formatter s;
  Format.fprintf Format.std_formatter "@.";
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
