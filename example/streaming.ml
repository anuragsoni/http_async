open Core
open Async

let handler req =
  let open Async_http in
  let body = req.Request.body in
  let length, pipe = Body.length body, Body.to_string_pipe body in
  let response_body =
    Pipe.create_reader ~close_on_exception:true (fun writer ->
        Pipe.transfer pipe writer ~f:String.uppercase)
  in
  let response = Response.make ~body:(Body.of_string_pipe ?length response_body) `OK in
  Deferred.Or_error.return response
;;

let main port =
  let where_to_listen = Tcp.Where_to_listen.of_port port in
  Async_connection.(Server.create ~on_handler_error:`Raise where_to_listen)
    (Async_http.Server.create_connection_handler handler)
  >>= fun _server -> Deferred.never ()
;;

let () =
  Command.async
    ~summary:"Streaming server"
    Command.Param.(
      map
        (flag "-p" (optional_with_default 8080 int) ~doc:"int Server port number")
        ~f:(fun port () -> main port))
  |> Command.run
;;
