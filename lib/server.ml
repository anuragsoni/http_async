open! Core
open Async

type request_handler = Request.t -> Response.t Deferred.t

let to_httpaf_handler request_handler =
  let handler _addr reqd =
    let req = Httpaf.Reqd.request reqd in
    let headers = req.Httpaf.Request.headers in
    let headers = Headers.of_list (Httpaf.Headers.to_list headers) in
    let meth = req.Httpaf.Request.meth in
    let body = Body.empty in
    let request = { Request.target = req.Httpaf.Request.target; headers; meth; body } in
    don't_wait_for
      (let%bind response = request_handler request in
       let headers =
         Httpaf.Headers.of_rev_list @@ Headers.to_list response.Response.headers
       in
       let status = response.Response.status in
       let r = Httpaf.Response.create ~headers status in
       match response.Response.body with
       | `Empty -> return (Httpaf.Reqd.respond_with_string reqd r "")
       | `Bigstring b -> return (Httpaf.Reqd.respond_with_bigstring reqd r b)
       | `String s -> return (Httpaf.Reqd.respond_with_string reqd r s)
       | `Stream s ->
         let response_body = Httpaf.Reqd.respond_with_streaming reqd r in
         upon (Pipe.closed s) (fun () -> Httpaf.Body.close_writer response_body);
         Pipe.iter_without_pushback s ~f:(fun { Core.Unix.IOVec.buf; pos; len } ->
             Httpaf.Body.schedule_bigstring ~len ~off:pos response_body buf))
  in
  handler
;;

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
    (Server0.create_connection_handler
       ~request_handler:(to_httpaf_handler request_handler)
       ~error_handler)
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
          ~request_handler:(to_httpaf_handler request_handler)
          ~error_handler
          _socket
          reader
          writer)
;;
