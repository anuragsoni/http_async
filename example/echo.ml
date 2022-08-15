open! Core
open! Async
open Http_async

let () =
  Command_unix.run
    (Server.run_command ~summary:"echo" (fun request ->
       let pipe = Service.body request in
       Service.respond_stream pipe))
;;
