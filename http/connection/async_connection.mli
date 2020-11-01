open! Core
open! Async

val log : Log.t

module Server : sig
  val create
    :  ?buffer_age_limit:Writer.buffer_age_limit
    -> ?max_connections:int
    -> ?max_accepts_per_batch:int
    -> ?backlog:int
    -> ?socket:([ `Unconnected ], Socket.Address.Inet.t) Socket.t
    -> [ `Call of Socket.Address.Inet.t -> exn -> unit | `Ignore | `Raise ]
    -> Tcp.Where_to_listen.inet
    -> (Reader.t -> Writer.t -> unit Deferred.t)
    -> (Socket.Address.Inet.t, int) Tcp.Server.t

  val create_ssl
    :  ?buffer_age_limit:Writer.buffer_age_limit
    -> ?max_connections:int
    -> ?max_accepts_per_batch:int
    -> ?backlog:int
    -> ?socket:([ `Unconnected ], Socket.Address.Inet.t) Socket.t
    -> ?version:Async_ssl.Ssl.Version.t
    -> ?options:Async_ssl.Ssl.Opt.t list
    -> ?name:string
    -> ?allowed_ciphers:[ `Only of string list | `Openssl_default | `Secure ]
    -> ?ca_file:string
    -> ?ca_path:string
    -> ?verify_modes:Async_ssl.Ssl.Verify_mode.t list
    -> crt_file:string
    -> key_file:string
    -> [ `Call of Socket.Address.Inet.t -> exn -> unit | `Ignore | `Raise ]
    -> Tcp.Where_to_listen.inet
    -> (Reader.t -> Writer.t -> unit Deferred.t)
    -> (Socket.Address.Inet.t, int) Tcp.Server.t
end
