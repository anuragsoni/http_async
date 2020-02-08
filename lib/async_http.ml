open! Core
open Async

module Server = struct
  let listen
      ?buffer_age_limit
      ?max_connections
      ?max_accepts_per_batch
      ?backlog
      ?socket
      ~on_handler_error
      ~request_handler
      ~error_handler
      where_to_listen
    =
    Tcp.(
      Server.create
        ?buffer_age_limit
        ?max_connections
        ?max_accepts_per_batch
        ?backlog
        ?socket
        ~on_handler_error
        where_to_listen)
      (Server0.create_connection_handler ~request_handler ~error_handler)
  ;;
end

module Client = Client
