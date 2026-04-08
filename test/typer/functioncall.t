functioncall typer checks:
  $ dune exec ola functioncall_1.lua
  Typing error: attempt to call a nil value
  $ dune exec ola functioncall_2.lua
  Typing error: attempt to call a non function value
  $ dune exec ola functioncall_3.lua
  File "functioncall_3.lua", line 5, char 10: Typing error: attempt to index a non table value
