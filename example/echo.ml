open! Core
open! Async
open Http_async

let () =
  Command_unix.run
    (Server.run_command ~summary:"echo" (fun (_request, body) ->
       let response = Response.create `Ok in
       return (response, Body.Writer.stream (Body.Reader.pipe body))))
;;
