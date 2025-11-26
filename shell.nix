{ pkgs ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/nixos-unstable.tar.gz";
  }) {}
}:

pkgs.mkShell {
  name = "synchronizer-dev-shell";
  dontDetectOcamlConflicts = true;
  nativeBuildInputs = with pkgs.ocamlPackages; [
    dune_3
    findlib
    bisect_ppx
    mdx
    merlin
    sedlex
    menhir
    ocaml
    ocamlformat
    odoc
  ];
  buildInputs = with pkgs.ocamlPackages; [
    sedlex
    menhirLib
  ];
}
