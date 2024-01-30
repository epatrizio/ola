variable shadowing:
  $ dune exec ola callingSideways.lua
  interprete ...
  f, outer a and b
  1
  2
  in g, outer a and b
  1
  2
  inner a and b
  300
  400
  back to f, inner a and b
  100
  200
  back to outside scope
  1
  2
