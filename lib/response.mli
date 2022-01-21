open! Core
open! Shuttle_http

type t = Server.response [@@deriving sexp_of]

val headers : t -> Http.Header.t
val status : t -> Http.Status.t
val body : t -> Body.Writer.t
val string : ?headers:Http.Header.t -> ?status:Http.Status.t -> string -> t
