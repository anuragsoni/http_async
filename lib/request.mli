open! Core

type t [@@deriving sexp_of]

val create : Http.Request.t -> Shuttle_http.Body.Reader.t -> t
val headers : t -> Http.Header.t
val meth : t -> Http.Method.t
val resource : t -> string
val body : t -> Shuttle_http.Body.Reader.t
