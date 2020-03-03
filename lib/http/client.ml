open Core
open Async

let connection_param_of_uri ssl_options uri =
  let host =
    Result.of_option
      (Uri.host uri)
      ~error:
        (Error.create
           "Could not determine host from uri"
           ("uri", uri)
           [%sexp_of: string * Uri_sexp.t])
  in
  let scheme =
    Result.of_option
      (Uri.scheme uri)
      ~error:
        (Error.create
           "Could not determine uri scheme"
           ("uri", uri)
           [%sexp_of: string * Uri_sexp.t])
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
      Error
        (Error.create
           "Could not determine port"
           ("uri", uri)
           [%sexp_of: string * Uri_sexp.t])
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

let%test_unit "create connection params" =
  let make = connection_param_of_uri @@ Async_connection.Client.create_ssl_options () in
  [%test_result: bool] (make (Uri.of_string "foobar") |> Or_error.is_error) ~expect:true;
  [%test_result: Host_and_port.t]
    (make (Uri.of_string "https://httpbin.org") |> Or_error.ok_exn |> snd)
    ~expect:(Host_and_port.create ~host:"httpbin.org" ~port:443);
  [%test_result: Host_and_port.t]
    (make (Uri.of_string "ws://httpbin.org") |> Or_error.ok_exn |> snd)
    ~expect:(Host_and_port.create ~host:"httpbin.org" ~port:80)
;;

let response_handler request_method resp finished response response_body =
  let length =
    match Httpaf.Response.body_length ~request_method response with
    | `Fixed l -> Some l
    | _ -> None
  in
  let body =
    Body.read_httpaf_body
      ?length
      (fun () ->
        Ivar.fill finished ();
        Httpaf.Body.close_reader response_body)
      response_body
  in
  Ivar.fill resp (Httpaf_http.httpaf_response_to_response response body)
;;

let write_body body request_body =
  match body with
  | None -> Deferred.unit
  | Some s ->
    (match s with
    | Body.Stream s ->
      Pipe.iter_without_pushback
        ~continue_on_error:true
        ~f:(fun { Core.Unix.IOVec.buf; pos; len } ->
          Httpaf.Body.write_bigstring request_body ~off:pos ~len buf;
          Httpaf.Body.flush request_body (fun () -> ()))
        s
    | String s ->
      Httpaf.Body.write_string request_body s;
      Deferred.unit
    | Bigstring s ->
      Httpaf.Body.write_bigstring request_body s;
      Deferred.unit
    | Empty -> Deferred.unit)
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
        (match Body.length b with
        | Some len ->
          Httpaf.Headers.add_unless_exists headers "Content-length" (Int64.to_string len)
        | None -> Httpaf.Headers.add_unless_exists headers "transfer-encoding" "chunked")
    in
    let request =
      Httpaf.Request.create ~headers (meth :> Httpaf.Method.t) (Uri.path_and_query uri)
    in
    let finished = Ivar.create () in
    let resp = Ivar.create () in
    let err_iv = Ivar.create () in
    let error_handler err =
      let e =
        match err with
        | `Malformed_response msg ->
          Or_error.error_s [%message "Malformed response" ~message:msg]
        | `Invalid_response_body_length _resp ->
          Or_error.error_s [%message "Invalid response body length"]
        | `Exn exn -> Or_error.of_exn exn
      in
      Ivar.fill err_iv e
    in
    don't_wait_for
      (Async_connection.Client.with_connection
         mode
         (Tcp.Where_to_connect.of_host_and_port host_and_port)
         (fun _addr reader writer ->
           let request_body =
             Protocol.Client.request
               ~response_handler:(response_handler meth resp finished)
               ~error_handler
               request
               reader
               writer
           in
           let%bind () = write_body (Option.map ~f:Body.content body) request_body in
           Httpaf.Body.close_writer request_body;
           Ivar.read finished));
    Deferred.any [ Ivar.read resp; Ivar.read err_iv ]
;;

let get ?ssl_options ?headers uri = request ?ssl_options ?headers `GET uri

let post ?ssl_options ?headers ?(body = Body.empty) uri =
  request ?ssl_options ?headers ~body `POST uri
;;
