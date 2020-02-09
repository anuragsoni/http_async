open Core
open Async

let run_client r w ~uri ~error_handler ~meth ~headers =
  let resp = Ivar.create () in
  let read_body body =
    let on_eof' () =
      Httpaf.Body.close_reader body;
      return @@ `Finished ()
    in
    let on_read' writer buffer ~off ~len =
      let fragment_copy = Bigstringaf.copy ~off ~len buffer in
      Pipe.write_if_open writer fragment_copy >>= fun () -> return @@ `Repeat ()
    in
    Pipe.create_reader ~close_on_exception:false (fun writer ->
        Deferred.repeat_until_finished () (fun () ->
            let next_iter = Ivar.create () in
            let on_eof () =
              don't_wait_for (on_eof' () >>| fun n -> Ivar.fill next_iter n)
            in
            let on_read buffer ~off ~len =
              don't_wait_for
                (on_read' writer buffer ~off ~len >>| fun n -> Ivar.fill next_iter n)
            in
            Httpaf.Body.schedule_read body ~on_eof ~on_read;
            Ivar.read next_iter))
  in
  let response_handler response response_body =
    let body = read_body response_body in
    Ivar.fill_if_empty resp (response, body)
  in
  let request = Httpaf.Request.create ~headers (meth :> Httpaf.Method.t) (Uri.path uri) in
  let body = Client0.request ~error_handler ~response_handler request r w in
  Httpaf.Body.close_writer body;
  Ivar.read resp
;;

let verify_cert conn =
  let open Async_ssl in
  match Ssl.Connection.peer_certificate conn with
  | None -> return false
  | Some (Error _) -> return false
  | Some (Ok _) -> return true
;;

let ssl_connect
    ?version
    ?allowed_ciphers
    ?options
    ?verify_modes
    ?ca_file
    ?ca_path
    ?(verify = verify_cert)
    r
    w
  =
  let open Async_ssl in
  let net_to_ssl, ssl_to_net = Io_util.pipes_from_reader_writer r w in
  let app_to_ssl, app_writer = Pipe.create () in
  let app_reader, ssl_to_app = Pipe.create () in
  Ssl.client
    ?version
    ?allowed_ciphers
    ?options
    ?verify_modes
    ?ca_file
    ?ca_path
    ~app_to_ssl
    ~ssl_to_app
    ~net_to_ssl
    ~ssl_to_net
    ()
  >>= function
  | Error error -> Io_util.close_reader_writer r w >>= fun () -> Error.raise error
  | Ok conn ->
    Io_util.pipes_to_reader_writer app_reader app_writer
    >>= fun (reader, writer) ->
    verify conn
    >>= (function
    | true -> return (conn, reader, writer)
    | false ->
      Io_util.close_reader_writer reader writer
      >>= fun () -> Error.raise (Error.of_string "Failed to validate certificate"))
;;

let connect
    ?buffer_age_limit
    ?interrupt
    ?reader_buffer_size
    ?writer_buffer_size
    ?timeout
    ?version
    ?allowed_ciphers
    ?options
    ?verify_modes
    ?ca_file
    ?ca_path
    ?verify
    uri
    f
  =
  let host = Option.value_exn ~here:[%here] (Uri.host uri) in
  let is_tls =
    match Uri.scheme uri with
    | Some "https" | Some "wss" -> true
    | _ -> false
  in
  let port =
    match Uri.port uri with
    | None ->
      (match Uri_services.tcp_port_of_uri uri with
      | Some p -> Some p
      | None -> None)
    | Some p -> Some p
  in
  let port = Option.value_exn ~here:[%here] port in
  Tcp.(
    connect
      ?buffer_age_limit
      ?interrupt
      ?reader_buffer_size
      ?writer_buffer_size
      ?timeout
      (Tcp.Where_to_connect.of_host_and_port (Host_and_port.create ~host ~port))
    >>= fun (_, r, w) ->
    if is_tls
    then
      ssl_connect
        ?version
        ?allowed_ciphers
        ?options
        ?verify_modes
        ?ca_file
        ?ca_path
        ?verify
        r
        w
      >>= fun (_conn, r, w) -> f host r w
    else f host r w)
;;

let request
    ~error_handler
    ~meth
    ?(headers = Httpaf.Headers.empty)
    ?buffer_age_limit
    ?interrupt
    ?reader_buffer_size
    ?writer_buffer_size
    ?version
    ?allowed_ciphers
    ?options
    ?verify_modes
    ?ca_file
    ?ca_path
    ?verify
    uri
  =
  connect
    ?buffer_age_limit
    ?interrupt
    ?reader_buffer_size
    ?writer_buffer_size
    ?version
    ?allowed_ciphers
    ?options
    ?verify_modes
    ?ca_file
    ?ca_path
    ?verify
    uri
    (fun host r w ->
      let headers = Httpaf.Headers.add_unless_exists headers "host" host in
      run_client r w ~uri ~meth ~headers ~error_handler
      >>| fun (response, body) ->
      don't_wait_for
        (Pipe.closed body >>= fun () -> Reader.close r >>= fun () -> Writer.close w);
      response, Body.of_stream body)
;;
