open Core
open Async

module R = struct
  open Routes
  open Async_http

  let not_found = Response.of_string ~status:`Not_found "Not found"

  let routes =
    one_of
      [ (nil @--> fun _req -> Response.of_string "Hello World")
      ; ((s "greet" / str /? nil)
        @--> fun a req ->
        let message = "Hello, " ^ a in
        Log.Global.info_s ([%sexp_of: Request.t] req);
        Response.of_string message)
      ]
  ;;
end

let request_handler req =
  let open Async_http in
  match Routes.match' ~target:req.Request.target R.routes with
  | None -> Error.raise @@ Error.of_string "foo"
  | Some r -> r req
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
