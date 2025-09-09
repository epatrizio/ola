variadic function static analysis checks:
  $ dune exec ola variadic_func.lua
  interprete ...
  File "variadic_func.lua", line 3, char 0: Static analysis error: cannot use '...' outside a vararg function near '...'
