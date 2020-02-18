open Async

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
