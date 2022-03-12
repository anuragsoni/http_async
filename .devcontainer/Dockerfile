FROM ocaml/opam:ubuntu-20.04-ocaml-4.13

USER root
RUN apt-get update && apt-get install -qq git inotify-tools libffi-dev zlib1g-dev libgmp-dev

USER opam

RUN opam pin add -n shuttle.dev git+https://github.com/anuragsoni/shuttle.git
RUN opam pin add -n http.dev git+https://github.com/mirage/ocaml-cohttp.git
RUN opam install shuttle http ocaml-lsp-server core_unix
RUN opam install ppx_log core_bench