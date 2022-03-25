open! Core
open! Async
open! Shuttle
open Eager_deferred.Use

type request = Request.t * Body.Reader.t
type response = Response.t * Body.Writer.t
type handler = request -> response Deferred.t
type error_handler = ?exn:Exn.t -> Status.t -> response Deferred.t

let keep_alive headers =
  match Headers.(find headers connection) with
  | Some x when String.Caseless.equal x "close" -> false
  | _ -> true
;;

let write_response writer encoding res =
  let module Writer = Output_channel in
  Writer.write writer (Version.to_string (Response.version res));
  Writer.write_char writer ' ';
  Writer.write writer (Status.to_string (Response.status res));
  Writer.write_char writer ' ';
  Writer.write writer "\r\n";
  let headers = Response.headers res in
  let headers =
    match encoding with
    | `Fixed len ->
      Headers.add_unless_exists
        headers
        ~key:Headers.content_length
        ~data:(Int.to_string len)
    | `Chunked ->
      Headers.add_unless_exists headers ~key:Headers.transfer_encoding ~data:"chunked"
  in
  Headers.iter
    ~f:(fun ~key ~data ->
      Writer.write writer key;
      Writer.write writer ": ";
      Writer.write writer data;
      Writer.write writer "\r\n")
    headers;
  Writer.write writer "\r\n"
;;

let default_error_handler ?exn:_ status =
  Service.respond_string
    ~headers:[ Headers.connection, "close"; Headers.content_length, "0" ]
    ~status
    ""
;;

let run_server_loop ?(error_handler = default_error_handler) handle_request reader writer =
  let monitor = Monitor.create () in
  let finished = Ivar.create () in
  let rec loop reader writer handle_request =
    let view = Input_channel.view reader in
    match Parser.parse_request view.buf ~pos:view.pos ~len:view.len with
    | Error Partial ->
      Input_channel.refill reader
      >>> (function
      | `Ok -> loop reader writer handle_request
      | `Eof | `Buffer_is_full -> Ivar.fill finished ())
    | Error (Fail error) ->
      Logger.debug "Error while parsing HTTP request: %s" (Error.to_string_mach error);
      error_handler `Bad_request
      >>> fun (res, res_body) ->
      write_response writer (Body.Writer.encoding res_body) res;
      Body.Writer.Private.write res_body writer
      >>> fun () ->
      Output_channel.schedule_flush writer;
      Ivar.fill finished ()
    | Ok (req, consumed) ->
      Input_channel.consume reader consumed;
      (match Body.Reader.Private.create req reader with
      | Error _error ->
        Logger.debug "Invalid request body";
        error_handler `Bad_request
        >>> fun (res, res_body) ->
        write_response writer (Body.Writer.encoding res_body) res;
        Body.Writer.Private.write res_body writer
        >>> fun () ->
        Output_channel.schedule_flush writer;
        Ivar.fill finished ()
      | Ok req_body ->
        handle_request (req, req_body)
        >>> fun (res, res_body) ->
        let keep_alive =
          keep_alive (Request.headers req) && keep_alive (Response.headers res)
        in
        write_response writer (Body.Writer.encoding res_body) res;
        Body.Writer.Private.write res_body writer
        >>> fun () ->
        Body.Reader.drain req_body
        >>> fun () ->
        if keep_alive then loop reader writer handle_request else Ivar.fill finished ())
  in
  (Monitor.detach_and_get_next_error monitor
  >>> fun exn ->
  error_handler ~exn `Internal_server_error
  >>> fun (res, res_body) ->
  write_response writer (Body.Writer.encoding res_body) res;
  Body.Writer.Private.write res_body writer >>> fun () -> Ivar.fill finished ());
  Scheduler.within ~priority:Priority.Normal ~monitor (fun () ->
      loop reader writer handle_request);
  Ivar.read finished
;;

let run
    ?(where_to_listen = Tcp.Where_to_listen.of_port 8080)
    ?max_connections
    ?(max_accepts_per_batch = 64)
    ?backlog
    ?socket
    ?initial_buffer_size
    ?error_handler
    service
  =
  Shuttle.Connection.listen
    ?input_buffer_size:initial_buffer_size
    ?output_buffer_size:initial_buffer_size
    ?max_connections
    ?backlog
    ?socket
    ~max_accepts_per_batch
    where_to_listen
    ~on_handler_error:`Raise
    ~f:(fun _addr reader writer -> run_server_loop ?error_handler service reader writer)
;;

let run_command ?(interrupt = Deferred.never ()) ?readme ?error_handler ~summary service =
  Command.async
    ~summary
    ?readme
    Command.Let_syntax.(
      let%map_open port =
        flag "-port" ~doc:"int Source port to listen on" (optional_with_default 8080 int)
      and max_connections =
        flag
          "-max-connections"
          ~doc:"int Maximum number of active connections"
          (optional int)
      and max_accepts_per_batch =
        flag
          "-max-accepts-per-batch"
          ~doc:"int Maximum number of connections to accept per Unix.accept call."
          (optional_with_default 64 int)
      and backlog =
        flag
          "-backlog"
          ~doc:"int Number of clients that can have a pending connection."
          (optional int)
      and initial_buffer_size =
        flag
          "-initial-buffer-size"
          ~doc:"int Initial size of the Read and Write buffers used by the server."
          (optional int)
      in
      fun () ->
        let%bind.Deferred server =
          run
            ?error_handler
            ~where_to_listen:(Tcp.Where_to_listen.of_port port)
            ~max_accepts_per_batch
            ?max_connections
            ?backlog
            ?initial_buffer_size
            service
        in
        choose
          [ choice interrupt (fun () -> `Shutdown)
          ; choice (Tcp.Server.close_finished_and_handlers_determined server) (fun () ->
                `Closed)
          ]
        >>= function
        | `Shutdown -> Tcp.Server.close ~close_existing_connections:true server
        | `Closed -> Deferred.unit)
;;
