open Core
open Async

module Server = struct
  let create_ssl handler ~crt_file ~key_file r w =
    let open Async_ssl in
    let net_to_ssl = Reader.pipe r in
    let ssl_to_net = Writer.pipe w in
    let app_to_ssl, app_writer = Pipe.create () in
    let app_reader, ssl_to_app = Pipe.create () in
    match%bind
      Ssl.server ~crt_file ~key_file ~net_to_ssl ~ssl_to_net ~ssl_to_app ~app_to_ssl ()
    with
    | Error e ->
      Log.Global.error_s (Error.sexp_of_t e);
      return ()
    | Ok conn ->
      let%bind reader =
        Reader.of_pipe (Info.of_string "async_connection.ssl.reader") app_reader
      in
      let%bind writer, `Closed_and_flushed_downstream flush =
        Writer.of_pipe (Info.of_string "async_connection.ssl.writer") app_writer
      in
      let shutdown () =
        let%bind () = Writer.close writer in
        let%bind () = flush in
        (* [Ssl.Connection.close] will cleanup and shutdown all the pipes
           provided to [Ssl.server]. *)
        Ssl.Connection.close conn;
        let%bind () =
          match%map Ssl.Connection.closed conn with
          | Ok _ -> ()
          | Error e -> Log.Global.error "Error happened: %s" (Error.to_string_hum e)
        in
        Reader.close reader
      in
      Monitor.protect
        ~here:[%here]
        ~name:"async_connection.ssl.server"
        ~finally:shutdown
        (fun () -> handler reader writer)
  ;;

  let create
      ?crt_file
      ?key_file
      ?buffer_age_limit
      ?max_connections
      ?max_accepts_per_batch
      ?backlog
      ?socket
      ~on_handler_error
      where_to_listen
      handle_client
    =
    Tcp.Server.create
      ?buffer_age_limit
      ?max_connections
      ?max_accepts_per_batch
      ?backlog
      ?socket
      ~on_handler_error
      where_to_listen
      (fun addr r w ->
        match crt_file, key_file with
        | Some crt_file, Some key_file ->
          create_ssl (handle_client addr) ~crt_file ~key_file r w
        | _ -> handle_client addr r w)
  ;;
end
