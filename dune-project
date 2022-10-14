(lang dune 2.9)

(name http_async)

(generate_opam_files true)

(source
 (github anuragsoni/http_async))

(authors "Anurag Soni <anurag@sonianurag.com>")

(maintainers "Anurag Soni <anurag@sonianurag.com>")

(documentation "https://anuragsoni.github.io/http_async/")

(license MIT)

(package
 (name http_async)
 (synopsis "Async library for HTTP/1.1 servers")
 (description
  "http_async implements an efficient HTTP/1.1 server. It uses the shuttle library for network IO, and provides an easy-to-use interface for writing async http services while minimal overhead on top of the user provided http handler.")
 (tags
  (http-server http http1.1 async))
 (depends
  (ocaml
   (>= 4.11.0))
  (shuttle
   (>= 0.6.0))
  (ppxlib
   (>= 0.23.0))))
