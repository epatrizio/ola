# ola - An OCaml Lua language interpreter

This project is an interpreter of the [Lua 5.4](https://www.lua.org/manual/5.4/manual.html) programming language
written in [OCaml](https://ocaml.org) with the [Dune](https://dune.build) build system.

- Build standard command: `dune build @all`
- Execute [Cram test](https://dune.readthedocs.io/en/stable/tests.html): `dune test` or `dune runtest`
- Run: `dune exec ola -- file_name.lua`
- Run in debug mode: `dune exec ola -- file_name.lua --debug`

<!-- $MDX file=examples/hello.lua -->
```lua
local function hello(msg)
  print("Hello, "..msg.."!")
end

hello("world")
```

```sh
$ dune exec ola -- examples/hello.lua
interprete ...
Hello, world!
```

- Install in current [opam OCaml switch](https://ocaml.org/docs/opam-switch-introduction): `dune install`
- Run: `ola file_name.lua` - `ola file_name.lua --debug`

Debug mode displays the Lua input file twice in the console: original version - after scope analysis version
(variables renamed with a unique name). Nb. Display formatting needs to be improved
([Issue](https://github.com/epatrizio/ola/issues/2)).

To enhance the test suite, I imported one from the experimental
[lua-wasm](https://github.com/Qcode/lua-wasm) project. Thanks to the author.
By the way, this is a very interesting exploratory work on compiling Lua to WebAssembly!

## Why ?

This experimental project has the following goals:

1. Write an interpreter allows a deep understanding of a programming language.
(Read a language specification in detail, understand the semantic, the execution model, etc.)
2. Learn how to deal with a dynamique language, such as typing at runtime.
3. Last but not least, Have fun!

Lua seems to be a good dynamic language for this context.\
Designed by a university research team ([PUC Rio Brazil](https://www.puc-rio.br)),
its syntax is minimalist and its semantics well defined.

## Current status

Implementing a whole Lua interpreter is a very big deal (a good experience to understand that),
I'm so far from it. For now, there are the basics, including the fundamental concepts of tables and closures
and the very beginnings of StdLib.
This already allows to do a lot of fun stuffs :-) #WIP

- See [examples](https://github.com/epatrizio/ola/tree/main/examples)
- See [releases](https://github.com/epatrizio/ola/releases)

## Contribute

*More fun in a group than alone!*

Feel free: Contact me, suggest issues and pull requests.

Thanks [@zapashcanon](https://github.com/zapashcanon), an active member of the OCaml community,
for first feedbacks and reviews.
