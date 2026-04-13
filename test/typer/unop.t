unop typer checks:
  $ dune exec ola bitwise_unop_float.lua
  File "bitwise_unop_float.lua", line 4, char 6: Evaluation error: number has no integer representation: 42.5
  $ dune exec ola bitwise_unop_func.lua
  File "bitwise_unop_func.lua", line 5, char 6: Typing error: attempt to perform bitwise operation on a function value
  $ dune exec ola bitwise_unop_table.lua
  File "bitwise_unop_table.lua", line 3, char 6: Typing error: attempt to perform bitwise operation on a table value

