(lang dune 3.11)
(name eio-trace)
(formatting disabled)
(generate_opam_files true)
(license ISC)
(source (github ocaml-multicore/eio-trace))
(maintainers "Thomas Leonard")
(authors "Thomas Leonard")
(package
 (name eio-trace)
 (synopsis "Trace visualisation for Eio programs")
 (depends
  (ocaml (>= 5.1.0))
  processor
  (eio_main (>= 0.14))
  (cmdliner (>= 1.2.0))
  (lablgtk3 (>= 3.1.4))
  (crowbar (and (>= 0.2.1) :with-test))))
