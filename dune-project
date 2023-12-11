(lang dune 3.11)
(name eio-trace)
(formatting disabled)
(generate_opam_files true)
(source (github talex5/eio-trace))
(authors "Thomas Leonard")
(package
 (name eio-trace)
 (synopsis "Trace visualisation for Eio programs")
 (depends
  (ocaml (>= 5.1.0))
  (eio (>= 0.13))
  (lablgtk3 (>= 3.1.3))))
