open Core
open Async

val log_src : Logs.Src.t

module Server : sig
  (** [create] delegates to Async's [Tcp.Server.create]. It
      forwards a reader & writer to the callback function, and the
      reader & writer will be closed if the deferred returned by the callback
      is fulfilled, or if there is an exception during the process. In addition,
      if the user provides the location for both public (crt_file) and private (key_file)
      an SSL connection is setup instead.
  *)
  val create
    :  ?crt_file:string
    -> ?key_file:string
    -> ?buffer_age_limit:Writer.buffer_age_limit
    -> ?max_connections:int
    -> ?max_accepts_per_batch:int
    -> ?backlog:int
    -> ?socket:([ `Unconnected ], ([< Socket.Address.t ] as 'a)) Socket.t
    -> on_handler_error:[ `Call of 'a -> exn -> unit | `Ignore | `Raise ]
    -> ('a, 'b) Tcp.Where_to_listen.t
    -> ('a -> Reader.t -> Writer.t -> unit Deferred.t)
    -> ('a, 'b) Tcp.Server.t Deferred.t
end

module Client : sig
  open Async_ssl

  type ssl_options =
    { version : Version.t option
    ; options : Opt.t list option
    ; name : string option
    ; hostname : string option
    ; allowed_ciphers : [ `Only of string list | `Openssl_default | `Secure ] option
    ; ca_file : string option
    ; ca_path : string option
    ; crt_file : string option
    ; key_file : string option
    ; verify_modes : Verify_mode.t list option
    ; session : (Ssl.Session.t[@sexp.opaque]) option
    ; verify_peer : Ssl.Connection.t -> unit Or_error.t
    }
  [@@deriving sexp_of, fields]

  val create_ssl_options
    :  ?version:Version.t
    -> ?options:Opt.t list
    -> ?name:string
    -> ?hostname:string
    -> ?allowed_ciphers:[ `Only of string list | `Openssl_default | `Secure ]
    -> ?ca_file:string
    -> ?ca_path:string
    -> ?crt_file:string
    -> ?key_file:string
    -> ?verify_modes:Verify_mode.t list
    -> ?session:Ssl.Session.t
    -> ?verify_peer:(Ssl.Connection.t -> unit Or_error.t)
    -> unit
    -> ssl_options

  type mode =
    | Secure of ssl_options
    | Regular
  [@@deriving sexp_of]

  val with_connection
    :  mode
    -> ?buffer_age_limit:[ `At_most of Core__.Core_time_float.Span.t | `Unlimited ]
    -> ?interrupt:unit Deferred.t
    -> ?reader_buffer_size:int
    -> ?writer_buffer_size:int
    -> ?timeout:Time.Span.t
    -> ([< Socket.Address.t ] as 'a) Tcp.Where_to_connect.t
    -> (([ `Active ], 'a) Socket.t -> Reader.t -> Writer.t -> 'b Async.Deferred.t)
    -> 'b Async.Deferred.t
end
