open! Core
open! Async
open Async_http

let () =
  Command_unix.run
    (Server.run_command ~summary:"Hello world HTTP Server" (fun request ->
         let%bind () =
           Pipe.iter_without_pushback
             ~f:(fun chunk -> Log.Global.info "%s" chunk)
             (Service.body request)
         in
         Service.respond_string "Hello World"))
;;
