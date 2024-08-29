(lang dune 3.0)

(using mdx 0.2)

(name ola)

(cram enable)

(using menhir 2.1)

(license Unlicense)

(authors "Eric Patrizio <epatrizio@mpns.fr>")

(maintainers "Eric Patrizio <epatrizio@mpns.fr>")

(source
 (github epatrizio/ola))

(generate_opam_files true)

(implicit_transitive_deps false)

(package
 (name ola)
 (synopsis "OCaml Lua Interpreter")
 (description
  "OCaml Lua Interpreter. Experimental project, work in progress ...")
 (tags
  (ola ocaml lua interpreter))
 (depends
  (ocaml
   (>= 4.14))
  sedlex
  (menhir
   (and
    :build
    (>= 20220210)))
  (bisect_ppx
   (and
    (>= 2.5)
    :with-dev-setup))
  (ocb
   (and
    :with-test
    (>= "0.1")
    :dev))
  (odoc :with-doc)
  (mdx
   (and
    :with-test
    (>= "2.1")))))
