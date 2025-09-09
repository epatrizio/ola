multi assignment function example:
  $ dune exec ola multipleAssignment.lua
  lexing parsing ...
  static analyses ...
  stdlib loading ...
  scope analysis ...
  interprete ...
  Test 1 -----
  nil
  nil
  nil
  nil
  Test 2 ------
  100
  101
  nil
  nil
  Test 3 ------
  1
  100
  101
  nil
  Test 4 ------
  1
  2
  100
  101
  Test 5 ------
  1
  2
  3
  100
  Test 6 ------
  1
  2
  3
  4
