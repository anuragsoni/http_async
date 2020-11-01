(lang dune 2.1)

(name async_http)

(generate_opam_files true)

(source
 (uri "git+https://git.sr.ht/~soni/async-http"))

(authors "Anurag Soni <anurag@sonianurag.com>")

(maintainers "Anurag Soni <anurag@sonianurag.com>")

(license MIT)

(package
 (name async_http)
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
  ppx_log
  httpaf
  faraday
  (async_ssl
   (>= 0.13))))
