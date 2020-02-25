open Core
open Async

let connection_param_of_uri ssl_options uri =
  let host =
    Result.of_option
      (Uri.host uri)
      ~error:
        (Error.create_s
           [%message "Could not determine host from uri" ~uri:(uri : Uri_sexp.t)])
  in
  let scheme =
    Result.of_option
      (Uri.scheme uri)
      ~error:
        (Error.create_s
           [%message "Could not determine uri scheme" ~uri:(uri : Uri_sexp.t)])
  in
  let open Or_error.Let_syntax in
  let%bind host = host
  and scheme = scheme in
  let%bind port =
    match Uri.port uri, Uri_services.tcp_port_of_uri uri, scheme with
    | Some p, _, _ -> Ok p
    | _, Some p, _ -> Ok p
    | _, _, "ws" -> Ok 80
    | _, _, "wss" -> Ok 443
    | _ ->
      Error (Error.create_s [%message "Could not determine port" ~uri:(uri : Uri_sexp.t)])
  in
  let host_and_port = Host_and_port.create ~host ~port in
  let%map mode =
    match scheme with
    | "http" | "ws" -> Ok Async_connection.Client.Regular
    | "https" | "wss" -> Ok (Async_connection.Client.Secure ssl_options)
    | _ -> Error (Error.create_s [%message "invalid uri scheme" ~scheme])
  in
  mode, host_and_port
;;

let response_handler resp finished response response_body =
  let body = Body.read_httpaf_body finished response_body in
  Ivar.fill resp (Or_error.return (response, body))
;;

let write_body body request_body =
  match body with
  | None -> Deferred.unit
  | Some body ->
    (match body with
    | Body.Empty -> Deferred.unit
    | String s ->
      Httpaf.Body.write_string request_body s;
      Deferred.unit
    | Bigstring { Core.Unix.IOVec.buf; pos; len } ->
      Httpaf.Body.write_bigstring request_body ~off:pos ~len buf;
      Deferred.unit
    | Stream s ->
      Pipe.iter_without_pushback
        ~continue_on_error:true
        ~f:(fun { Core.Unix.IOVec.buf; pos; len } ->
          Httpaf.Body.write_bigstring request_body ~off:pos ~len buf)
        s)
;;

let request
    ?(ssl_options = Async_connection.Client.create_ssl_options ())
    ?(headers = Httpaf.Headers.empty)
    ?body
    meth
    uri
  =
  match connection_param_of_uri ssl_options uri with
  | Error err -> Deferred.Or_error.fail err
  | Ok (mode, host_and_port) ->
    let headers =
      Httpaf.Headers.add_unless_exists headers "Host" (Host_and_port.host host_and_port)
    in
    let headers =
      match body with
      | None -> headers
      | Some b ->
        (match Body.kind b with
        | Body.Fixed len ->
          Httpaf.Headers.add_unless_exists headers "Content-length" (Int64.to_string len)
        | Chunked ->
          Httpaf.Headers.add_unless_exists headers "transfer-encoding" "chunked")
    in
    let request = Httpaf.Request.create ~headers meth (Uri.path_and_query uri) in
    let finished = Ivar.create () in
    let resp = Ivar.create () in
    don't_wait_for
      (Async_connection.Client.with_connection
         mode
         (Tcp.Where_to_connect.of_host_and_port host_and_port)
         (fun _addr reader writer ->
           let request_body =
             Protocol.Client.request
               ~response_handler:(response_handler resp finished)
               ~error_handler:(fun _ -> assert false)
               request
               reader
               writer
           in
           let%bind () = write_body (Option.map ~f:Body.content body) request_body in
           Httpaf.Body.close_writer request_body;
           Ivar.read finished));
    Ivar.read resp
;;

let get ?ssl_options ?headers uri = request ?ssl_options ?headers `GET uri

let post ?ssl_options ?headers ?(body = Body.empty) uri =
  request ?ssl_options ?headers ~body `POST uri
;;
