(lang dune 2.8)

(name current-web-pipelines)

(generate_opam_files true)

(source
 (github tarides/current-web-pipelines))

(authors "lucas@tarides.com")

(maintainers "lucas@tarides.com")

(package
 (name current-web-pipelines)
 (synopsis "Simplify the creation of pipeline websites")
 (description "")
 (depends
  (ocaml
   (>= 4.10))
  (current
   (>= 0.5))
  (current_web
   (>= 0.5))
  (current_git
   (>= 0.5))
  fmt
  routes
  (alcotest :with-test)))
