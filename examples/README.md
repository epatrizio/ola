# Examples

The objective of those lua scripts is to provide some classic functions using standard language features
to manipulate the `ola` interpreter in more advanced contexts than test suite.

- Mutually recursive even-odd functions : `dune exec ola ./examples/even_odd.lua`

- [Factorial multiple implementation](https://en.wikipedia.org/wiki/Factorial) :
`dune exec ola ./examples/factorial.lua`

- [Fibonacci sequence multiple implementation](https://en.wikipedia.org/wiki/Fibonacci_sequence) :
`dune exec ola ./examples/fibonacci.lua`

- [Conway's Game of Life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life) :
`dune exec ola ./examples/life.lua`

*Nb. The Game of Life calculations and processes are correct, but the display is not.*
*Todo: `io.write` and `os.execute` better implementation.*
