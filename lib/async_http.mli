open Async

module Http : sig
  module Server : sig
    val listen
      :  ?max_connections:int
      -> ?max_accepts_per_batch:int
      -> ?backlog:int
      -> ?socket:([ `Unconnected ], ([< Socket.Address.t ] as 'a)) Socket.t
      -> on_handler_error:[ `Call of 'a -> exn -> unit | `Ignore | `Raise ]
      -> request_handler:('a -> Httpaf.Server_connection.request_handler)
      -> error_handler:('a -> Httpaf.Server_connection.error_handler)
      -> ('a, 'b) Tcp.Where_to_listen.t
      -> ('a, 'b) Tcp.Server.t Deferred.t
  end
end
