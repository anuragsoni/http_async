open Core
open Async
open Async_http

let text = String.init 2053 ~f:(fun _ -> 'a')
let text = Bigstring.of_string text
let handler _request = Service.respond_bigstring text

let start_server port accepts () =
  Shuttle.Connection.listen
    ~input_buffer_size:0x4000
    ~output_buffer_size:0x4000
    ~backlog:11_000
    ~max_accepts_per_batch:accepts
    (Tcp.Where_to_listen.of_port port)
    ~on_handler_error:`Raise
    ~f:(fun _addr reader writer -> Server.run_server_loop handler reader writer)
  >>= fun server ->
  Deferred.forever () (fun () ->
      after Time.Span.(of_sec 0.5)
      >>| fun () ->
      Log.Global.printf "Active connections: %d" (Tcp.Server.num_connections server));
  Deferred.never ()
;;

let () =
  Command.async
    ~summary:"Start a hello world Async server"
    Command.Param.(
      map
        (both
           (flag
              "-p"
              (optional_with_default 8080 int)
              ~doc:"int Source port to listen on")
           (flag "-a" (optional_with_default 1 int) ~doc:"int Maximum accepts per batch"))
        ~f:(fun (port, accepts) () -> start_server port accepts ()))
  |> Command.run
;;
