(lang dune 1.11)
(using menhir 2.0)
(generate_opam_files true)

(name bibtex-website)
(source (github mreppen/bibtex-website))

(package
  (name bibtex-website)
  (synopsis "Insert bibtex references into html templates")
  (depends base stdio menhir))
