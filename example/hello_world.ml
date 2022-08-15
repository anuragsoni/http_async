open! Core
open! Async
open Http_async

let () =
  Command_unix.run
    (Server.run_command ~summary:"Hello world HTTP Server" (fun _request ->
       Service.respond_string "Hello World"))
;;
