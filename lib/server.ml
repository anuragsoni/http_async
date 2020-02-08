open! Core
open Async

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

let listen_ssl
    ?buffer_age_limit
    ?max_connections
    ?max_accepts_per_batch
    ?backlog
    ?socket
    ?version
    ?options
    ?name
    ?allowed_ciphers
    ?ca_file
    ?ca_path
    ?verify_modes
    ~on_handler_error
    ~request_handler
    ~error_handler
    ~crt_file
    ~key_file
    where_to_listen
  =
  let open Async_ssl in
  Tcp.(
    Server.create
      ?buffer_age_limit
      ?max_connections
      ?max_accepts_per_batch
      ?backlog
      ?socket
      ~on_handler_error
      where_to_listen)
    (fun _socket reader writer ->
      let net_to_ssl, ssl_to_net = Io_util.pipes_from_reader_writer reader writer in
      let app_to_ssl, app_writer = Pipe.create () in
      let app_reader, ssl_to_app = Pipe.create () in
      Ssl.server
        ?version
        ?options
        ?name
        ?allowed_ciphers
        ?ca_file
        ?ca_path
        ?verify_modes
        ~crt_file
        ~key_file
        ~net_to_ssl
        ~ssl_to_net
        ~ssl_to_app
        ~app_to_ssl
        ()
      >>= function
      | Error err ->
        Io_util.close_reader_writer reader writer >>= fun () -> Error.raise err
      | Ok conn ->
        Io_util.pipes_to_reader_writer app_reader app_writer
        >>= fun (reader, writer) ->
        don't_wait_for
          (Deferred.all_unit
             [ Reader.close_finished reader; Writer.close_finished writer ]
          >>| fun () -> Ssl.Connection.close conn);
        Server0.create_connection_handler
          ~request_handler
          ~error_handler
          _socket
          reader
          writer)
;;
