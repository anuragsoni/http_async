open Core
open Async

let run_client r w ~error_handler ~request =
  let read_body finished response body =
    let bs = Bigbuffer.create 0x100 in
    let on_eof () = Ivar.fill finished (response, Bigbuffer.contents bs) in
    let rec on_read buf ~off:pos ~len =
      Bigbuffer.add_bigstring bs (Bigstring.sub_shared buf ~pos ~len);
      Httpaf.Body.schedule_read body ~on_eof ~on_read
    in
    Httpaf.Body.schedule_read body ~on_eof ~on_read
  in
  let resp = Ivar.create () in
  let response_handler r response response_body = read_body r response response_body in
  let body =
    Client0.request ~error_handler ~response_handler:(response_handler resp) request r w
  in
  Httpaf.Body.close_writer body;
  Ivar.read resp
;;

let ssl_connect ?version ?allowed_ciphers ?options ?verify_modes ?ca_file ?ca_path r w =
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
    >>| fun (reader, writer) ->
    don't_wait_for
      (Deferred.all_unit [ Reader.close_finished reader; Writer.close_finished writer ]
      >>| fun () -> Ssl.Connection.close conn);
    conn, reader, writer
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
    with_connection
      ?buffer_age_limit
      ?interrupt
      ?reader_buffer_size
      ?writer_buffer_size
      ?timeout
      (Tcp.Where_to_connect.of_host_and_port (Host_and_port.create ~host ~port))
      (fun _ r w ->
        if is_tls
        then
          ssl_connect
            ?version
            ?allowed_ciphers
            ?options
            ?verify_modes
            ?ca_file
            ?ca_path
            r
            w
          >>= fun (_conn, r, w) -> f host r w
        else f host r w))
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
    uri
    (fun host r w ->
      let headers = Httpaf.Headers.add_unless_exists headers "host" host in
      let request = Httpaf.Request.create ~headers meth (Uri.path uri) in
      run_client r w ~request ~error_handler)
;;
