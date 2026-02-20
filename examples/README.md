# Examples

The objective of those Lua scripts is to provide some classic functions using standard language features
to manipulate the `ola` interpreter in more advanced contexts than test suite.

- Mutually recursive even-odd functions: `dune exec ola -- ./examples/even_odd.lua`

- [Factorial multiple implementation](https://en.wikipedia.org/wiki/Factorial):
`dune exec ola -- ./examples/factorial.lua`

- [Fibonacci sequence multiple implementation](https://en.wikipedia.org/wiki/Fibonacci_sequence):
`dune exec ola -- ./examples/fibonacci.lua`

- [Tower of Hanoi](https://en.wikipedia.org/wiki/Tower_of_Hanoi):
`dune exec ola -- ./examples/hanoi.lua`

- [Conway's Game of Life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life):
`dune exec ola -- ./examples/life.lua`

- [Oriented object programming](https://www.lua.org/pil/16.html), a beautiful application of tables and closures:
  - Basic example (and an application: classic functional programming Option and Result types):
    `dune exec ola -- ./examples/oop.lua`
  - Advanced example 1 (Class definition, new instance and inheritance):
    `dune exec ola -- ./examples/oop_advanced_1.lua`
  - Advanced example 2 (Same 1 with privacy elements - attributes and functions):
    `dune exec ola -- ./examples/oop_advanced_2.lua`

- Custom implementation of *iparis* and *pairs* stdlib iterators. As an application,
implementation of classic functions *table_map*, *table_filter*, *table_fold_left*,
*table_iter*, *table_iteri* and *table_find*:
`dune exec ola -- ./examples/table.lua`

- [Metatables and Metamethods](https://www.lua.org/pil/13.html):
how to make a table read-only `dune exec ola -- ./examples/metatable_cst.lua`

- [Data structures](https://www.lua.org/pil/11.html):
*Tables in Lua are not a data structure; they are THE data structure!*\
With Lua Tables (associated with meta-mechanisms), it's possible to easly implement all data structures.
For example, [Sets end Bags](https://www.lua.org/pil/11.5.html) are explained in the official documentation.
Here are some custom implemented examples (experimental, incomplete, but informative - I hope so :-):
  - Array: `dune exec ola -- ./examples/data_structures/array.lua`
  - Binary Search Tree (BST): `dune exec ola -- ./examples/data_structures/binary_search_tree.lua`
  - Linked list and Stack (as linked list application example): `dune exec ola -- ./examples/data_structures/linked_list.lua`
  - Queue: `lua ./examples/data_structures/queue.lua`
  - Set: `dune exec ola -- ./examples/data_structures/set.lua`

*Nb. Examples are part of the cram test suite except for Queue (Table stdlib not fully implemented)*
