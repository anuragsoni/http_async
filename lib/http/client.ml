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
  let on_eof' () =
    Httpaf.Body.close_reader response_body;
    Ivar.fill finished ();
    return @@ `Finished ()
  in
  let on_read' writer b ~off ~len =
    let module Unix = Core.Unix in
    let%map () = Pipe.write_if_open writer (Unix.IOVec.of_bigstring ~pos:off ~len b) in
    `Repeat ()
  in
  let body =
    (* Async recommends choosing false for [close_on_exception]. In a normal flow,
       closing the write end of the pipe will indicate that the writer finished successfully. *)
    Pipe.create_reader ~close_on_exception:false (fun writer ->
        (* [create_reader] will automatically close the writer end, when this Deferred becomes
           determined. We loop here so we can process a chain of Httpaf read events. *)
        Deferred.repeat_until_finished () (fun () ->
            let next_iter = Ivar.create () in
            let on_eof () =
              don't_wait_for (on_eof' () >>| fun n -> Ivar.fill next_iter n)
            in
            let on_read buffer ~off ~len =
              don't_wait_for
                (on_read' writer buffer ~off ~len >>| fun n -> Ivar.fill next_iter n)
            in
            Httpaf.Body.schedule_read response_body ~on_eof ~on_read;
            Ivar.read next_iter))
  in
  Ivar.fill resp (Or_error.return (response, Body.stream body))
;;

let request
    ?(ssl_options = Async_connection.Client.default_ssl_options)
    ?(headers = Httpaf.Headers.empty)
    meth
    uri
  =
  match connection_param_of_uri ssl_options uri with
  | Error err -> Deferred.Or_error.fail err
  | Ok (mode, host_and_port) ->
    let headers =
      Httpaf.Headers.add_unless_exists headers "Host" (Host_and_port.host host_and_port)
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
           Httpaf.Body.close_writer request_body;
           Ivar.read finished));
    Ivar.read resp
;;

let get ?ssl_options ?headers uri = request ?ssl_options ?headers `GET uri
