(lang dune 2.1)

(name async-http)

(generate_opam_files true)

(source
 (github anuragsoni/async-http))

(authors "Anurag Soni <anurag@sonianurag.com>")

(maintainers "Anurag Soni <anurag@sonianurag.com>")

(license MIT)

(package
 (name async-http)
 (synopsis "Async web toolkit")
 (tags
  (http-server http-client http async ocaml))
 (depends
  (ocaml
   (>= 4.08.0))
  (dune
   (>= 2.1))
  (core
   (>= 0.13))
  (async
   (>= 0.13))
  ppx_jane
  httpaf
  (routes :with-dev)
  faraday
  (async_ssl
   (>= 0.13))
  uri))
