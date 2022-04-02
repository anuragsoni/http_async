(lang dune 2.9)

(name async_http)

(generate_opam_files true)

(source
 (github anuragsoni/async-http))

(authors "Anurag Soni <anurag@sonianurag.com>")

(maintainers "Anurag Soni <anurag@sonianurag.com>")

(license MIT)

(package
 (name async_http)
 (synopsis "Async web toolkit")
 (tags
  (http-server http async ocaml))
 (depends
  (ocaml
   (>= 4.11.0))
  (shuttle
   (>= 0.4.0))
  (ppxlib
   (>= 0.23.0))))
