open Async_kernel

type t = Request.t -> Response.t Deferred.t
