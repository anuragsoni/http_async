open Core
open Async

let handler _req =
  let open Async_http in
  let headers =
    Headers.of_list
      [ Headers.content_length (Bigstring.length Test_data.text |> Int.to_int64) ]
  in
  let response = Response.make ~headers ~body:(Body.of_bigstring Test_data.text) `OK in
  Deferred.Or_error.return response
;;

let main port =
  let where_to_listen = Tcp.Where_to_listen.of_port port in
  Async_connection.(
    Server.create
    (* ~crt_file:"./certs/localhost.pem" *)
    (* ~key_file:"./certs/localhost.key" *)
      ~on_handler_error:`Ignore
      where_to_listen)
    (Async_http.Server.create_connection_handler handler)
  >>= fun server ->
  Deferred.forever () (fun () ->
      Clock.after Time.Span.(of_sec 0.5)
      >>| fun () -> Log.Global.info "connections: %d" (Tcp.Server.num_connections server));
  Deferred.never ()
;;

let () =
  Command.async
    ~summary:"Sample server"
    Command.Param.(
      map
        (flag "-p" (optional_with_default 8080 int) ~doc:"int Server port number")
        ~f:(fun port () -> main port))
  |> Command.run
;;
