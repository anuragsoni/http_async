open Core
open Async

let handler req =
  let open Async_http in
  match String.split ~on:'/' req.Request.target with
  | [ ""; "hello" ] -> Response.of_string "Hello World!"
  | [ ""; "bigstring" ] -> Response.of_bigstring Test_data.text
  | [ ""; "greet"; name ] when String.length name > 0 ->
    Response.of_string (sprintf "Hello, %s" name)
  | [ ""; "file" ] -> Response.of_file "./test/sample.html"
  | [ ""; "stream" ] ->
    (* In a real application this is where one can check if the client request
       is using the appropriate HTTP method. *)
    let body = req.Request.body in
    (* [length] can be empty if the request transfer encoding was chunked and no
       actual length was provided in the header. *)
    let length, pipe = Body.length body, Body.to_pipe body in
    (* We can use any of the async pipe utilities to create a streaming response
       body. *)
    let response_body =
      Pipe.create_reader ~close_on_exception:true (fun writer ->
          Pipe.transfer pipe writer ~f:String.uppercase)
    in
    return (Response.make ~body:(Body.of_pipe ?length response_body) `OK)
  | _ -> Response.of_string ~status:`Not_found "Route not found"
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
