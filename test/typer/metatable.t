metatable typer checks:
  $ dune exec ola metatable_1.lua
  Interpretation error: Typing error: bad argument #1 to 'setmetatable' (nil or table expected)
  $ dune exec ola metatable_2.lua
  Interpretation error: Typing error: bad argument #1 to 'setmetatable' (nil or table expected)
  $ dune exec ola metatable_3.lua
  Interpretation error: Typing error: bad argument #2 to 'setmetatable' (nil or table expected)
  $ dune exec ola metatable_4.lua
  Interpretation error: Typing error: bad argument #2 to 'setmetatable' (nil or table expected)
  $ dune exec ola metatable_5.lua
  protected metatable!
  Interpretation error: cannot change a protected metatable
  $ dune exec ola metatable_6.lua
  Interpretation error: Typing error: metatable.__tostring: attempt to call a non function value
  $ dune exec ola metatable_7.lua
  Interpretation error: metatable.__index: attempt to index a non table or function value
  $ dune exec ola metatable_8.lua
  Interpretation error: metatable.__newindex: attempt to index a non table or function value
  $ dune exec ola metatable_9.lua
  Interpretation error: Typing error: metatable.__pairs: attempt to call a non function value
