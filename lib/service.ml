open Async

type t = Request.t -> Response.t Deferred.t
