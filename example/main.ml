open Core
open Async

let request_handler _ =
  let open Async_http in
  Response.of_file "./test/big.txt"
;;

let error_handler _ ?request:_ error start_response =
  let open Httpaf in
  let response_body = start_response Headers.empty in
  (match error with
  | `Exn exn ->
    Body.write_string response_body (Exn.to_string exn);
    Body.write_string response_body "\n"
  | #Status.standard as error ->
    Body.write_string response_body (Status.default_reason_phrase error));
  Body.close_writer response_body
;;

let main port () =
  let where_to_listen =
    Tcp.Where_to_listen.bind_to
      Tcp.Bind_to_address.Localhost
      (Tcp.Bind_to_port.On_port port)
  in
  Async_http.Server.listen_ssl
    ~on_handler_error:`Ignore
    ~request_handler
    ~crt_file:"./certs/localhost.pem"
    ~key_file:"./certs/localhost.key"
    ~error_handler
    where_to_listen
  >>= fun server ->
  Log.Global.info "Listening on http://localhost:%d" port;
  Deferred.forever () (fun () ->
      Clock.after Time.Span.(of_sec 0.5)
      >>| fun () -> Log.Global.info "conns: %d" (Tcp.Server.num_connections server));
  Deferred.never ()
;;

let () =
  Log.Global.set_level (Log.Level.of_string "Info");
  Command.async
    ~summary:"Sample server"
    Command.Param.(
      map
        (flag "-p" (optional_with_default 8080 int) ~doc:"int Source port to listen on")
        ~f:(fun port () -> main port ()))
  |> Command.run
;;
