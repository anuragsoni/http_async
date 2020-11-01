open! Core
open! Async
open Async_ssl
open Ppx_log_async
module Logger = Log.Make_global ()

let log = Lazy.force Logger.log

module Server = struct
  let create_internal
      ?buffer_age_limit
      ?max_connections
      ?max_accepts_per_batch
      ?backlog
      ?socket
      on_handler_error
      where_to_listen
      handler
    =
    Tcp.Server.create_inet
      ?buffer_age_limit
      ?max_connections
      ?max_accepts_per_batch
      ?backlog
      ?socket
      ~on_handler_error
      where_to_listen
      (fun socket reader writer ->
        match%map
          Monitor.try_with ~here:[%here] (fun () -> handler socket reader writer)
        with
        | Ok () -> ()
        | Error err -> [%log.error log "Unhandled exception" (err : Exn.t)])
  ;;

  let create_ssl
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
      ~crt_file
      ~key_file
      on_handler_error
      where_to_listen
      handler
    =
    let handler _ reader writer =
      let net_to_ssl = Reader.pipe reader in
      let ssl_to_net = Writer.pipe writer in
      let app_to_ssl, app_writer = Pipe.create () in
      let app_reader, ssl_to_app = Pipe.create () in
      match%bind
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
      with
      | Error e ->
        [%log.error log (e : Error.t)];
        return ()
      | Ok conn ->
        let%bind reader =
          Reader.of_pipe (Info.of_string "async_http.ssl_reader") app_reader
        in
        let%bind writer, `Closed_and_flushed_downstream flush =
          Writer.of_pipe (Info.of_string "async_http.ssl_writer") app_writer
        in
        let shutdown () =
          let%bind () = Writer.close writer in
          let%bind () = flush in
          Ssl.Connection.close conn;
          let%bind () =
            match%map Ssl.Connection.closed conn with
            | Ok _ -> ()
            | Error e -> [%log.error log (e : Error.t)]
          in
          Reader.close reader
        in
        Monitor.protect
          ~here:[%here]
          ~name:"async_http.ssl.server"
          ~finally:shutdown
          (fun () -> handler reader writer)
    in
    create_internal
      ?buffer_age_limit
      ?max_connections
      ?max_accepts_per_batch
      ?backlog
      ?socket
      on_handler_error
      where_to_listen
      handler
  ;;

  let create
      ?buffer_age_limit
      ?max_connections
      ?max_accepts_per_batch
      ?backlog
      ?socket
      on_handler_error
      where_to_listen
      handler
    =
    let handler _ reader writer = handler reader writer in
    create_internal
      ?buffer_age_limit
      ?max_connections
      ?max_accepts_per_batch
      ?backlog
      ?socket
      on_handler_error
      where_to_listen
      handler
  ;;
end
