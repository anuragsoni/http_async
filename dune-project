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
 (tags
  (http-server http http1.1 async))
 (depends
  (ocaml
   (>= 4.11.0))
  (shuttle
   (>= 0.5.0))
  (ppxlib
   (>= 0.23.0))))
