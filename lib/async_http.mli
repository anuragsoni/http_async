open Async
open Httpaf

module Body : sig
  type t

  val empty : t
  val of_string : string -> t
  val to_string_stream : t -> string Pipe.Reader.t
  val to_string : t -> string Deferred.t
  val drain : t -> unit Deferred.t
end

module Server : sig
  val listen
    :  ?buffer_age_limit:Writer.buffer_age_limit
    -> ?max_connections:int
    -> ?max_accepts_per_batch:int
    -> ?backlog:int
    -> ?socket:([ `Unconnected ], ([< Async.Socket.Address.t ] as 'a)) Async.Socket.t
    -> on_handler_error:[ `Call of 'a -> exn -> unit | `Ignore | `Raise ]
    -> request_handler:('a -> Server_connection.request_handler)
    -> error_handler:('a -> Server_connection.error_handler)
    -> ('a, 'b) Tcp.Where_to_listen.t
    -> ('a, 'b) Tcp.Server.t Deferred.t

  val listen_ssl
    :  ?buffer_age_limit:Writer.buffer_age_limit
    -> ?max_connections:int
    -> ?max_accepts_per_batch:int
    -> ?backlog:int
    -> ?socket:([ `Unconnected ], ([< Async.Socket.Address.t ] as 'a)) Async.Socket.t
    -> ?version:Async_ssl.Version.t
    -> ?options:Async_ssl.Opt.t list
    -> ?name:string
    -> ?allowed_ciphers:[ `Only of string list | `Openssl_default | `Secure ]
    -> ?ca_file:string
    -> ?ca_path:string
    -> ?verify_modes:Async_ssl.Verify_mode.t list
    -> on_handler_error:[ `Call of 'a -> exn -> unit | `Ignore | `Raise ]
    -> request_handler:('a -> Server_connection.request_handler)
    -> error_handler:('a -> Server_connection.error_handler)
    -> crt_file:string
    -> key_file:string
    -> ('a, 'b) Tcp.Where_to_listen.t
    -> ('a, 'b) Tcp.Server.t Deferred.t
end

module Client : sig
  val request
    :  error_handler:Client_connection.error_handler
    -> meth:Method.standard
    -> ?headers:Headers.t
    -> ?buffer_age_limit:Writer.buffer_age_limit
    -> ?interrupt:unit Deferred.t
    -> ?reader_buffer_size:int
    -> ?writer_buffer_size:int
    -> ?version:Async_ssl.Version.t
    -> ?allowed_ciphers:[ `Only of string list | `Openssl_default | `Secure ]
    -> ?options:Async_ssl.Opt.t list
    -> ?verify_modes:Async_ssl.Verify_mode.t list
    -> ?ca_file:string
    -> ?ca_path:string
    -> Uri.t
    -> (Response.t * Body.t) Deferred.t
end
