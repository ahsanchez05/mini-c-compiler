(lang dune 3.0)
(using menhir 2.1)

(name mini_c_compiler)

(generate_opam_files true)

(package
 (name mini_c_compiler)
 (synopsis "Compiler for Cigrid, a small C-like language")
 (depends
  ocaml
  dune
  menhir))