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
   (>= 4.08.0))
  shuttle
  (http (> 5.0.0))
  (ppx_jane
   (and
    (>= v0.14)
    (< v0.15)))
  ppx_log))
