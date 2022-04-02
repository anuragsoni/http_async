open! Core
open! Async
open! Shuttle
module Logger : Log.Global_intf
module Status = Status
module Request = Request
module Response = Response
module Meth = Meth
module Headers = Headers

(** [Service] is the core abstraction that represents an HTTP server within http_async.*)
module Service : sig
  type request [@@deriving sexp_of]
  type response [@@deriving sexp_of]

  (** [t] is a function that takes a HTTP request and returns a deferred HTTP response. *)
  type t = request -> response Deferred.t

  (** [resource] returns the path and query for a given request. *)
  val resource : request -> string

  (** [meth] returns the HTTP verb for a given request. *)
  val meth : request -> Meth.t

  (** [body] returns the HTTP request body for a given request. *)
  val body : request -> string Pipe.Reader.t

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
    -> ?status:Status.t
    -> string
    -> response Deferred.t

  (** [respond_bigstring] creates a new fixed length encoded response from the user
      provided [Bigstring.t]. If the user provided headers don't contain a Content-Length
      header, one is added with the value set to the bigstring's length. *)
  val respond_bigstring
    :  ?headers:(string * string) list
    -> ?status:Status.t
    -> Bigstring.t
    -> response Deferred.t

  (** [respond_stream] creates a new streaming response. The data is chunk-encoded before
      its sent over the wire. *)
  val respond_stream
    :  ?headers:(string * string) list
    -> ?status:Status.t
    -> string Pipe.Reader.t
    -> response Deferred.t
end

module Server : sig
  type error_handler = ?exn:Exn.t -> Status.t -> Service.response Deferred.t

  (** [run_server_loop] accepts a HTTP service, and returns a callback that can be used to
      drive the server loop created via [Shuttle.Connection.listen]. This allows the user
      to customize the [Input_channel] and [Output_channel] and have control over the
      various Server configuration options like [accept_n], [backlog] and more. *)
  val run_server_loop
    :  ?error_handler:error_handler
    -> Service.t
    -> Input_channel.t
    -> Output_channel.t
    -> unit Deferred.t

  (** [run] sets up a [Tcp.Server.t] and drives the HTTP server loop with the user
      provided [Service.t]. *)
  val run
    :  ?where_to_listen:Tcp.Where_to_listen.inet
    -> ?max_connections:int
    -> ?max_accepts_per_batch:int
    -> ?backlog:int
    -> ?socket:([ `Unconnected ], Socket.Address.Inet.t) Socket.t
    -> ?initial_buffer_size:int
    -> ?error_handler:error_handler
    -> Service.t
    -> (Socket.Address.Inet.t, int) Tcp.Server.t Deferred.t

  (** [run_command] is similar to [run] but instead returns an [Async.Command.t] that can
      be used to start the async event loop from a program's entrypoint. If [interrupt] is
      provided, the server will be stopped when [interrupt] is fulfilled. *)
  val run_command
    :  ?interrupt:unit Deferred.t
    -> ?readme:(unit -> string)
    -> ?error_handler:error_handler
    -> summary:string
    -> Service.t
    -> Command.t
end

module Private : sig
  module Parser = Parser
end
