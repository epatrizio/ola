unop typer checks:
  $ dune exec ola bitwise_unop_func.lua
  interprete ...
  File "bitwise_unop_func.lua", line 5, char 7: Typing error: attempt to perform bitwise operation on a function value
  $ dune exec ola bitwise_unop_table.lua
  interprete ...
  File "bitwise_unop_table.lua", line 3, char 7: Typing error: attempt to perform bitwise operation on a table value

