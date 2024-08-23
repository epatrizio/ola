# ola - An OCaml Lua language interpreter

This project is an interpreter of the [Lua 5.4](https://www.lua.org/manual/5.4/manual.html) programming language
written in [OCaml](https://ocaml.org) with the [Dune](https://dune.build) build system.

- Build standard command: `dune build @all`
- Execute [Cram test](https://dune.readthedocs.io/en/stable/tests.html): `dune test`
- Run examples: `dune exec ola ./examples/file_name.lua`

<!-- $MDX file=examples/hello.lua -->
```lua
local function hello(msg)
  print("Hello, "..msg.."!")
end

hello("world")
```

```sh
$ dune exec ola examples/hello.lua
interprete ...
Hello, world!
```

## Why ?

This experimental project has only two objectives (for now ;) :

1. Learn how to deal with a dynamique language, such as typing at runtime.
2. Have fun!

Lua seems to be a good dynamic language for this context.\
Designed by a university research team ([PUC Rio Brazil](https://www.puc-rio.br)),
its syntax is minimalist and its semantics well defined.

## Current status

## Contribute

*More fun in a group than alone!*

Feel free: Contact me, suggest issues and pull requests.

Thanks [@zapashcanon](https://github.com/zapashcanon), an active member of the OCaml community,
for first feedbacks and reviews. 
