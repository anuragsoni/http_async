open Core
open Async
open Async_http
open Httpaf

let text = "Hello World"

let request_handler _ =
  let headers =
    Headers.of_list [ "content-length", Int.to_string (String.length text) ]
  in
  let handler reqd =
    let request_body = Reqd.request_body reqd in
    Body.close_reader request_body;
    Reqd.respond_with_string reqd (Response.create ~headers `OK) text
  in
  handler
;;

let error_handler _ ?request:_ error start_response =
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
  let where_to_listen = Tcp.Where_to_listen.of_port port in
  Http.Server.listen
    ~on_handler_error:`Raise
    ~request_handler
    ~error_handler
    where_to_listen
  >>= fun server ->
  Log.Global.info "Listening on http://localhost:%d" port;
  Deferred.forever () (fun () ->
      Clock.after Time.Span.(of_sec 5.)
      >>| fun () -> Log.Global.info "conns: %d" (Tcp.Server.num_connections server));
  Deferred.never ()
;;

let () =
  Command.async
    ~summary:"Sample server"
    Command.Param.(
      map
        (flag "-p" (optional_with_default 8080 int) ~doc:"int Source port to listen on")
        ~f:(fun port () -> main port ()))
  |> Command.run
;;
