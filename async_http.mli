open Async

module Client : sig
  val connect
    :  error_handler:Httpaf.Client_connection.error_handler
    -> request:Httpaf.Request.t
    -> string Deferred.t
end
