  $ dune exec ola binary_search_tree.lua
  lexing parsing ...
  static analyses ...
  stdlib loading ...
  scope analysis ...
  interprete ...
  **** bst of integer ****
  nb nodes, 5
  nb leafs, 3
  min, 10
  max, 55
  -- print_prefix
  11
  10
  33
  22
  55
  -- print_infix
  10
  11
  22
  33
  55
  -- print_postfix
  10
  22
  55
  33
  11
  search 42, false
  search 22, true
  search 55, true
  -- delete 55 33 42 10
  nb nodes, 2
  nb leafs, 1
  min, 11
  max, 22
  search 55, false
  **** bst of string 1 ****
  min, aa
  max, zzz
  -- print_infix
  aa
  aaa
  str_1
  str_2
  zzz
  **** bst of string 2 ****
  min, aa
  max, str_2
  -- print_infix
  aa
  aaa
  zzz
  str_1
  str_2
