  $ dune exec ola metatable_cst.lua
  lexing parsing ...
  static analyses ...
  stdlib loading ...
  scope analysis ...
  interprete ...
  1. -----
  11
  22
  my_cst_value
  attempt to change constants, 1, 111
  attempt to change constants, 2, 222
  attempt to change constants, my_cst, new_value
  attempt to change constants, 3, 33
  attempt to change constants, my_new_cst, new_value
  nil
  nil
  2. -----
  11
  22
  my_cst_value
  attempt to change constant directly, 1, 111
  attempt to change constant directly, 2, 222
  attempt to change constant directly, my_cst, new_value
  attempt to change constant directly, 3, 33
  attempt to change constant directly, my_new_cst, new_value
  33
  new_value
