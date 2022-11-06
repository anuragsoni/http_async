open! Core
open! Async
open! Shuttle
module Logger : Log.Global_intf
module Status = Status
module Request = Request
module Version = Version
module Response = Response
module Meth = Meth
module Headers = Headers
module Buffer_config = Buffer_config

module Body : sig
  (** [Reader] represents streaming request bodies. Readers can work with both fixed
      length and chunk encoded bodies. *)
  module Reader : sig
    type t [@@deriving sexp_of]

    val encoding : t -> [ `Chunked | `Fixed of int ]
    val pipe : t -> Core.Bigstring.t Core_unix.IOVec.t Async.Pipe.Reader.t
  end

  module Writer : sig
    (** [Writer] represents response bodies. It supports both fixed length bodies
        represented via strings/bigstrings, and streaming bodies. *)
    type t [@@deriving sexp_of]

    val encoding : t -> [ `Chunked | `Fixed of int ]
    val empty : t
    val string : string -> t
    val bigstring : Core.Bigstring.t -> t

    val stream
      :  ?encoding:[ `Chunked | `Fixed of int ]
      -> Bigstring.t Core_unix.IOVec.t Async.Pipe.Reader.t
      -> t
  end
end

module Server : sig
  type error_handler =
    ?exn:Exn.t
    -> ?request:Request.t
    -> Status.t
    -> (Response.t * Body.Writer.t) Deferred.t

  (** [run_server_loop] accepts a HTTP service, and returns a callback that can be used to
      drive the server loop created via [Shuttle.Connection.listen]. This allows the user
      to customize the [Input_channel] and [Output_channel] and have control over the
      various Server configuration options like [accept_n], [backlog] and more. *)
  val run_server_loop
    :  ?error_handler:error_handler
    -> (Request.t * Body.Reader.t -> (Response.t * Body.Writer.t) Deferred.t)
    -> Input_channel.t
    -> Output_channel.t
    -> unit Deferred.t

  (** [run] sets up a [Tcp.Server.t] and drives the HTTP server loop with the user
      provided request-handler. *)
  val run
    :  ?max_connections:int
    -> ?max_accepts_per_batch:int
    -> ?backlog:int
    -> ?socket:([ `Unconnected ], ([< Socket.Address.t ] as 'a)) Socket.t
    -> ?buffer_config:Buffer_config.t
    -> ?error_handler:error_handler
    -> where_to_listen:('a, 'b) Tcp.Where_to_listen.t
    -> (Request.t * Body.Reader.t -> (Response.t * Body.Writer.t) Deferred.t)
    -> ('a, 'b) Tcp.Server.t Deferred.t

  (** [run_command] is similar to [run] but instead returns an [Async.Command.t] that can
      be used to start the async event loop from a program's entrypoint. If [interrupt] is
      provided, the server will be stopped when [interrupt] is fulfilled. *)
  val run_command
    :  ?interrupt:unit Deferred.t
    -> ?readme:(unit -> string)
    -> ?error_handler:error_handler
    -> summary:string
    -> (Request.t * Body.Reader.t -> (Response.t * Body.Writer.t) Deferred.t)
    -> Command.t
end

module Private : sig
  module Parser = Parser
end
