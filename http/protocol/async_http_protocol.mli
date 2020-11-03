open Async

module Server : sig
  val create_connection_handler
    :  ?config:Httpaf.Config.t
    -> ?error_handler:Httpaf.Server_connection.error_handler
    -> request_handler:Httpaf.Server_connection.request_handler
    -> Reader.t
    -> Writer.t
    -> unit Deferred.t
end
