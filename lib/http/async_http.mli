open Core
open Async

module Body : sig
  type iovec = Bigstring.t Core.Unix.IOVec.t [@@deriving sexp_of]
  type t [@@deriving sexp_of]

  val drain : t -> unit Deferred.t
  val to_string : t -> string Deferred.t
  val to_pipe : t -> iovec Pipe.Reader.t
end

module Server : sig
  val create_connection_handler
    :  ?config:Httpaf.Config.t
    -> request_handler:('a -> Httpaf.Server_connection.request_handler)
    -> ?error_handler:Httpaf.Server_connection.error_handler
    -> 'a
    -> Reader.t
    -> Writer.t
    -> unit Deferred.t
end

module Client : sig
  val request
    :  ?ssl_options:Async_connection.Client.ssl_options
    -> ?headers:Httpaf.Headers.t
    -> Httpaf.Method.t
    -> Uri.t
    -> (Httpaf.Response.t * Body.t) Deferred.Or_error.t

  val get
    :  ?ssl_options:Async_connection.Client.ssl_options
    -> ?headers:Httpaf.Headers.t
    -> Uri.t
    -> (Httpaf.Response.t * Body.t) Deferred.Or_error.t
end
