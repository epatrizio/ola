module LibMap = Map.Make (String)

let lib =
  let add_func lib_name func_name func lib =
    match LibMap.find_opt lib_name lib with
    | Some l ->
      let l = LibMap.add func_name func l in
      LibMap.add lib_name l lib
    | None -> assert false
  in
  let lib_basic = LibMap.empty in
  let lib_basic = LibMap.add "assert" Lua_stdlib_basic.asert lib_basic in
  let lib_basic = LibMap.add "ipairs" Lua_stdlib_basic.ipairs lib_basic in
  let lib_basic = LibMap.add "next" Lua_stdlib_basic.next lib_basic in
  let lib_basic = LibMap.add "pairs" Lua_stdlib_basic.pairs lib_basic in
  let lib_basic = LibMap.add "print" Lua_stdlib_basic.print lib_basic in
  let lib_basic = LibMap.add "type" Lua_stdlib_basic.typ lib_basic in
  let lib_basic = LibMap.add "tostring" Lua_stdlib_basic.tostring lib_basic in
  let lib = LibMap.empty in
  let lib = LibMap.add "math" LibMap.empty lib in
  let lib = add_func "math" "abs" Lua_stdlib_math.abs lib in
  let lib = add_func "math" "cos" Lua_stdlib_math.cos lib in
  let lib = add_func "math" "sin" Lua_stdlib_math.sin lib in
  let lib = add_func "math" "tan" Lua_stdlib_math.tan lib in
  let lib = add_func "math" "random" Lua_stdlib_math.random lib in
  let lib = LibMap.add "io" LibMap.empty lib in
  let lib = add_func "io" "write" Lua_stdlib_io.write lib in
  let lib = LibMap.add "os" LibMap.empty lib in
  let lib = add_func "os" "execute" Lua_stdlib_os.execute lib in
  (lib_basic, lib)
