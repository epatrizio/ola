# Documentation

*The documentation is generated automatically (.github/workflows/deploy.yml).*

## API

[odoc](https://github.com/ocaml/odoc) is a documentation generator tool for OCaml.
For now, `odoc` is installed but has not yet been implemented in `ola`. (#TODO)

View the current [HTML report](https://mpns.fr/ola/api/).

## Code coverage [![coverage-badge]][coverage status]

[Bisect_ppx](https://github.com/aantron/bisect_ppx) is a code coverage tool for OCaml.

```console
$ BISECT_FILE=$(pwd)/bisect opam exec -- dune runtest --force --instrument-with bisect_ppx
$ bisect-ppx-report summary
[202604] Manual coverage: 225/1379 (16.21%)
```

For now, coverage is low! The test suite covers all features, but a significant effort
is needed to cover all cases, all combinations of types for operations, etc.

View the current [HTML report](https://mpns.fr/ola/coverage/).

[coverage-badge]: https://mpns.fr/ola/coverage/badge.svg
[coverage status]: https://mpns.fr/ola/coverage/
