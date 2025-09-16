open Ast

let () = Random.self_init ()

let rec tostring_value v =
  match v with
  | Vnil () -> "nil"
  | Vboolean b -> string_of_bool b
  | Vnumber (Ninteger i) -> string_of_int i
  | Vnumber (Nfloat f) -> string_of_float f
  | Vstring s -> s
  | Vtable (i, tbl) as t -> begin
    match Table.get_metatable tbl with
    | Some mt -> begin
      match Table.get (fun _ -> None) (Vstring "__tostring") mt with
      | Some (Vfunction (_, _, _) as f) ->
        let env = Env.empty () in
        let _, v2, _ =
          match
            Interpret.interpret_fct f [ (Ast.empty_location (), Evalue t) ] env
          with
          | Error (_, msg) -> Lua_stdlib_common.error msg
          | Ok v -> v
        in
        tostring_value v2
      | Some _ ->
        Lua_stdlib_common.typing_error
          "metatable.__tostring: attempt to call a non function value"
      | None -> Format.sprintf "table: %i" (Int32.to_int i)
    end
    | None -> Format.sprintf "table: %i" (Int32.to_int i)
  end
  | Vfunction (i, _, _) | VfunctionStdLib (i, _) ->
    Format.sprintf "function: %i" (Int32.to_int i)
  | VfunctionReturn vl | Vvariadic vl -> (
    match vl with
    | [] -> ""
    | [ v ] -> tostring_value v
    | v :: tl ->
      Format.sprintf "%s, %s" (tostring_value v)
        (tostring_value (VfunctionReturn tl)) )

let asert v =
  begin
    match v with
    | Vnil () :: [ Vstring msg ] | Vboolean false :: [ Vstring msg ] ->
      assert (
        print_endline (Format.sprintf "assert: %s" msg);
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
    | [ Vtable (_, tbl); Vnumber (Ninteger i) ] when i > 0 -> begin
      match Table.next (Some (Table.Ikey i)) tbl with
      | Some (Table.Ikey i, v) -> [ Vnumber (Ninteger i); v ]
      | Some (Table.Kkey k, v) -> [ k; v ]
      | None -> [ Vnil () ]
    end
    | [ Vtable (_, tbl); v ] -> begin
      match Table.next (Some (Table.Kkey v)) tbl with
      | Some (Table.Ikey i, v) -> [ Vnumber (Ninteger i); v ]
      | Some (Table.Kkey k, v) -> [ k; v ]
      | None -> [ Vnil () ]
    end
    | _ -> assert false
  with Table.Table_error msg -> Lua_stdlib_common.error msg

let pairs v =
  match v with
  | [ Vtable (i, tbl) ] ->
    [ VfunctionStdLib (Random.bits32 (), next); Vtable (i, tbl); Vnil () ]
  | _ ->
    Lua_stdlib_common.typing_error
      "bad argument #1 to 'for iterator' (table expected)"

let inext v =
  match v with
  | [ Vtable (_, tbl) ] | [ Vtable (_, tbl); Vnil () ] -> begin
    match Table.inext 0 tbl with
    | Some (i, v) -> [ Vnumber (Ninteger i); v ]
    | None -> [ Vnil () ]
  end
  | [ Vtable (_, tbl); Vnumber (Ninteger i) ] when i > 0 -> begin
    match Table.inext i tbl with
    | Some (i, v) -> [ Vnumber (Ninteger i); v ]
    | None -> [ Vnil () ]
  end
  | _ -> assert false

let ipairs v =
  match v with
  | [ Vtable (i, tbl) ] ->
    [ VfunctionStdLib (Random.bits32 (), inext); Vtable (i, tbl); Vnil () ]
  | _ ->
    Lua_stdlib_common.typing_error
      "bad argument #1 to 'for iterator' (table expected)"

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

let getmetatable v =
  match v with
  | [ Vtable (_, tbl) ] -> begin
    match Table.get_metatable tbl with
    | Some mt -> begin
      match Table.get (fun _ -> None) (Vstring "__metatable") mt with
      | Some v -> [ v ]
      | None -> [ Vtable (Random.bits32 (), mt) ]
    end
    | None -> [ Vnil () ]
  end
  | _ -> [ Vnil () ]

let setmetatable v =
  match v with
  | Vtable (id, tbl) :: Vnil () :: _tl ->
    let tbl = Table.remove_metatable tbl in
    [ Vtable (id, tbl) ]
  | Vtable (id, tbl) :: Vtable (_, meta_tbl) :: _tl -> begin
    match Table.get_metatable tbl with
    | Some mt -> begin
      match Table.get (fun _ -> None) (Vstring "__metatable") mt with
      | Some _ -> Lua_stdlib_common.error "cannot change a protected metatable"
      | None ->
        let tbl = Table.set_metatable meta_tbl tbl in
        [ Vtable (id, tbl) ]
    end
    | None ->
      let tbl = Table.set_metatable meta_tbl tbl in
      [ Vtable (id, tbl) ]
  end
  | Vtable (_, _) :: _tl ->
    Lua_stdlib_common.typing_error
      "bad argument #2 to 'setmetatable' (nil or table expected)"
  | _ :: _tl ->
    Lua_stdlib_common.typing_error
      "bad argument #1 to 'setmetatable' (nil or table expected)"
  | [] ->
    Lua_stdlib_common.typing_error
      "bad argument #1 to 'setmetatable' (nil or table expected)"
