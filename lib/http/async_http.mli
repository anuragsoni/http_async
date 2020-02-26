open Core
open Async

module Body : sig
  type iovec = Bigstring.t Core.Unix.IOVec.t [@@deriving sexp_of]

  type length =
    private
    [ `Fixed of Int64.t
    | `Chunked
    | `Close_delimited
    | `Error of [ `Bad_gateway | `Internal_server_error ]
    | `Unknown
    ]
  [@@deriving sexp_of]

  type content = private
    | Empty
    | String of string
    | Bigstring of iovec
    | Stream of iovec Pipe.Reader.t
  [@@deriving sexp_of]

  type t = private
    { length : length
    ; content : content
    }
  [@@deriving sexp_of, fields]

  val drain : t -> unit Deferred.t
  val to_string : t -> string Deferred.t
  val to_pipe : t -> iovec Pipe.Reader.t
  val empty : t
  val of_string : string -> t
  val of_bigstring : Bigstring.t -> t
  val of_stream : ?length:length -> iovec Pipe.Reader.t -> t
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
    -> ?body:Body.t
    -> Httpaf.Method.standard
    -> Uri.t
    -> (Httpaf.Response.t * Body.t) Deferred.Or_error.t

  val get
    :  ?ssl_options:Async_connection.Client.ssl_options
    -> ?headers:Httpaf.Headers.t
    -> Uri.t
    -> (Httpaf.Response.t * Body.t) Deferred.Or_error.t

  val post
    :  ?ssl_options:Async_connection.Client.ssl_options
    -> ?headers:Httpaf.Headers.t
    -> ?body:Body.t
    -> Uri.t
    -> (Httpaf.Response.t * Body.t) Deferred.Or_error.t
end
