(lang dune 2.8)

(name current-web-pipelines)

(generate_opam_files true)

(source
 (github TheLortex/current-web-pipelines))

(authors "lucas@tarides.com")

(maintainers "lucas@tarides.com")

(package
 (name current-web-pipelines)
 (synopsis "Simplify the creation of pipeline websites")
 (description "")
 (depends
  (ocaml
   (>= 4.08.0))
  (current
   (>= 0.3))))
