open! Core
open! Async
open! Shuttle

type error_handler =
  ?exn:Exn.t -> ?request:Request.t -> Status.t -> (Response.t * Body.Writer.t) Deferred.t
[@@deriving sexp_of]

type service = Request.t * Body.Reader.t -> (Response.t * Body.Writer.t) Deferred.t
[@@deriving sexp_of]

let keep_alive headers =
  match Headers.find headers "connection" with
  | Some x when String.Caseless.equal x "close" -> false
  | _ -> true
;;

let write_response writer encoding res =
  Output_channel.write writer (Version.to_string (Response.version res));
  Output_channel.write_char writer ' ';
  Output_channel.write writer (Status.to_string (Response.status res));
  Output_channel.write_char writer ' ';
  Output_channel.write writer "\r\n";
  let headers = Response.headers res in
  let headers =
    match encoding with
    | `Fixed len ->
      Headers.add_unless_exists headers ~key:"Content-Length" ~data:(Int.to_string len)
    | `Chunked ->
      Headers.add_unless_exists headers ~key:"Transfer-Encoding" ~data:"chunked"
  in
  Headers.iter
    ~f:(fun ~key ~data ->
      Output_channel.write writer key;
      Output_channel.write writer ": ";
      Output_channel.write writer data;
      Output_channel.write writer "\r\n")
    headers;
  Output_channel.write writer "\r\n"
;;

let default_error_handler ?exn:_ ?request:_ status =
  let response =
    Response.create
      ~headers:(Headers.of_rev_list [ "Connection", "close"; "Content-Length", "0" ])
      status
  in
  return (response, Body.Writer.empty)
;;

module Context = struct
  type t =
    { service : service
    ; error_handler : error_handler
    ; reader : Input_channel.t
    ; writer : Output_channel.t
    ; requests : Request.t Moption.t
    ; closed : unit Ivar.t
    }
  [@@deriving sexp_of]

  let create service error_handler reader writer =
    { service
    ; error_handler
    ; reader
    ; writer
    ; requests = Moption.create ()
    ; closed = Ivar.create ()
    }
  ;;

  let report_error t exn status_code =
    if Ivar.is_full t.closed
    then (
      Logger.error !"Error handler invoked for a closed connection: %{sexp: Exn.t}" exn;
      Deferred.unit)
    else (
      let request = Moption.get t.requests in
      t.error_handler ?request ~exn status_code
      >>= fun (res, res_body) ->
      write_response t.writer (Body.Writer.encoding res_body) res;
      Body.Writer.Private.write res_body t.writer)
  ;;

  let rec server_loop t =
    let view = Input_channel.view t.reader in
    match Parser.parse_request view.buf ~pos:view.pos ~len:view.len with
    | Error Partial ->
      upon (Input_channel.refill t.reader) (function
        | `Ok -> server_loop t
        | `Eof ->
          Logger.debug "Unexpected EOF before a full request was parsed";
          Ivar.fill_if_empty t.closed ())
    | Error (Fail error) ->
      upon
        (report_error t (Error.to_exn error) `Bad_request)
        (fun () -> Ivar.fill_if_empty t.closed ())
    | Ok (req, consumed) ->
      Input_channel.consume t.reader consumed;
      Moption.set_some t.requests req;
      (match Body.Reader.Private.create req t.reader with
       | Error error ->
         upon
           (report_error t (Error.to_exn error) `Bad_request)
           (fun () -> Ivar.fill_if_empty t.closed ())
       | Ok req_body ->
         upon
           (t.service (req, req_body))
           (fun (res, res_body) ->
             let keep_alive =
               keep_alive (Request.headers req) && keep_alive (Response.headers res)
             in
             write_response t.writer (Body.Writer.encoding res_body) res;
             upon (Body.Writer.Private.write res_body t.writer) (fun () ->
               upon (Body.Reader.drain req_body) (fun () ->
                 if keep_alive
                 then (
                   Moption.set_none t.requests;
                   server_loop t)
                 else Ivar.fill_if_empty t.closed ()))))
  ;;
end

let run_server_loop ?(error_handler = default_error_handler) handle_request reader writer =
  let monitor = Monitor.create () in
  let context = Context.create handle_request error_handler reader writer in
  upon (Monitor.detach_and_get_next_error monitor) (fun exn ->
    upon (Context.report_error context exn `Internal_server_error) (fun () ->
      Ivar.fill_if_empty context.closed ()));
  Scheduler.within ~priority:Priority.Normal ~monitor (fun () ->
    Context.server_loop context);
  Ivar.read context.closed
;;

let run
  ?(where_to_listen = Tcp.Where_to_listen.of_port 8080)
  ?max_connections
  ?(max_accepts_per_batch = 64)
  ?backlog
  ?socket
  ?(buffer_config = Buffer_config.create ())
  ?error_handler
  service
  =
  Shuttle.Connection.listen
    ~input_buffer_size:(Buffer_config.initial_size buffer_config)
    ~max_input_buffer_size:(Buffer_config.max_buffer_size buffer_config)
    ~output_buffer_size:(Buffer_config.initial_size buffer_config)
    ~max_output_buffer_size:(Buffer_config.max_buffer_size buffer_config)
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
      and max_buffer_size =
        flag
          "-max-buffer-size"
          ~doc:"int Maximum size of the Read and Write buffers used by the server."
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
            ~buffer_config:
              (Buffer_config.create ?initial_size:initial_buffer_size ?max_buffer_size ())
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
