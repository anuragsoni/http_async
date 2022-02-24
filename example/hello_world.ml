open! Core
open! Async
open Async_http

let () =
  Command.run
    (Server.run_command ~summary:"Hello world HTTP Server" (fun _request ->
         Service.respond_string "Hello World"))
;;
