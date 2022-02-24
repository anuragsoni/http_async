open! Core
open! Async
open! Shuttle

(** Bodies for HTTP requests and responses, with support for streaming. *)
module Body : sig
  (** Streaming body reader. *)
  module Reader : sig
    type t [@@deriving sexp_of]

    (** [encoding] returns whether the body is a chunk encoded payload, or a fixed length
        payload. *)
    val encoding : t -> Http.Transfer.encoding

    (** [pipe] creates an async pipe and returns its reader end. This can be used for
        consuming a request body using the full API set provided by [Async_kernel.Pipe]. *)
    val pipe : t -> string Pipe.Reader.t

    (** [drain] will read chunks of the HTTP body and discard them. *)
    val drain : t -> unit Deferred.t
  end

  (** Body writer with support for streaming. *)
  module Writer : sig
    type t [@@deriving sexp_of]

    (** [encoding] returns whether the body is a chunk encoded payload, or a fixed length
        payload. *)
    val encoding : t -> Http.Transfer.encoding

    (** [empty] represents a fixed length encoded body of length 0. *)
    val empty : t

    (** [string] creates a fixed length body from the input [String.t]. *)
    val string : string -> t

    (** [bigstring] creates a fixed length body from the input [Bigstring.t]. *)
    val bigstring : Bigstring.t -> t

    (** [stream] creates a streaming body writer from the given pipe. Default value of
        [?encoding] is "chunked". The body writer ensures that the payloads will be chunk
        encoded when using an encoding value of chunked. *)
    val stream : ?encoding:Http.Transfer.encoding -> string Pipe.Reader.t -> t
  end
end

(** [Service] is the core abstraction that represents an HTTP server within async_http.*)
module Service : sig
  type request [@@deriving sexp_of]
  type response [@@deriving sexp_of]

  (** [t] is a function that takes a HTTP request and returns a deferred HTTP response. *)
  type t = request -> response Deferred.t

  (** [resource] returns the path and query for a given request. *)
  val resource : request -> string

  (** [body] returns the HTTP request body for a given request. *)
  val body : request -> Body.Reader.t

  (** [header request key] returns the last header value associates with [key] if one
      exists. *)
  val header : request -> string -> string option

  (** [header_multi request key] returns a list of all header values associated with
      [key]. *)
  val header_multi : request -> string -> string list

  (** [respond_string] creates a new fixed length encoded response from the user provided
      string. If the user provided headers don't contain a Content-Length header, one is
      added with the value set to the string's length. *)
  val respond_string
    :  ?headers:(string * string) list
    -> ?status:Http.Status.t
    -> string
    -> response Deferred.t

  (** [respond_bigstring] creates a new fixed length encoded response from the user
      provided [Bigstring.t]. If the user provided headers don't contain a Content-Length
      header, one is added with the value set to the bigstring's length. *)
  val respond_bigstring
    :  ?headers:(string * string) list
    -> ?status:Http.Status.t
    -> Bigstring.t
    -> response Deferred.t
end

module Server : sig
  (** [run_server_loop] accepts a HTTP service, and returns a callback that can be used to
      drive the server loop created via [Shuttle.Connection.listen]. This allows the user
      to customize the [Input_channel] and [Output_channel] and have control over the
      various Server configuration options like [accept_n], [backlog] and more. *)
  val run_server_loop
    :  Service.t
    -> Input_channel.t
    -> Output_channel.t
    -> unit Deferred.t
end

module Private : sig
  module Parser = Parser
end
