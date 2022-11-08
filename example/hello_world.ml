open! Core
open! Async
open Http_async

let () =
  Command_unix.run
    (Server.run_command ~summary:"Hello world HTTP Server" (fun addr (request, _body) ->
       Log.Global.info
         "(%s): %s"
         (Socket.Address.Inet.to_string addr)
         (Request.path request);
       return (Response.create `Ok, Body.Writer.string "Hello World")))
;;
