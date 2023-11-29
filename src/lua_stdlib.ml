module LibMap = Map.Make (String)

let lib =
  let add_func lib_name func_name func lib =
    match LibMap.find_opt lib_name lib with
    | Some l ->
      let l = LibMap.add func_name func l in
      LibMap.add lib_name l lib
    | None -> assert false
  in
  let lib = LibMap.empty in
  let lib = LibMap.add "basic" LibMap.empty lib in
  let lib = LibMap.add "math" LibMap.empty lib in
  let lib = add_func "math" "abs" Lua_stdlib_math.abs lib in
  lib
