open! Core
open! Async
open Async_http

let handler request =
  match Service.resource request with
  | "/" -> Service.respond_string "Hello World"
  | _ -> Service.respond_string ~headers:[ "connection", "close" ] ~status:`Not_found ""
;;

let start_server port () =
  Shuttle.Connection.listen
    (Tcp.Where_to_listen.of_port port)
    ~on_handler_error:`Raise
    ~f:(fun _addr reader writer -> Server.run_server_loop handler reader writer)
  >>= fun _server -> Deferred.never ()
;;

let command =
  Command.async
    ~summary:"Start a hello world Async server"
    Command.Let_syntax.(
      let%map_open port =
        flag "-p" ~doc:"int Source port to listen on" (optional_with_default 8080 int)
      in
      start_server port)
;;

let () = Command.run command
