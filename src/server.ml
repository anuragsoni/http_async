open! Core
open! Async
open! Shuttle
open Eager_deferred.Use
module Logger = Log.Make_global ()

type request = Http.Request.t * Body.Reader.t
type response = Http.Response.t * Body.Writer.t
type handler = request -> response Deferred.t

let write_response writer encoding res =
  let module Writer = Output_channel in
  let open Http in
  Writer.write writer (Version.to_string (Response.version res));
  Writer.write_char writer ' ';
  Writer.write writer (Status.to_string (Response.status res));
  Writer.write_char writer ' ';
  Writer.write writer "\r\n";
  let headers = Response.headers res in
  Header.iter
    (fun key data ->
      (* TODO: Should this raise an exception if the user provides invalid set of headers,
         i.e. multiple content-lengths, invalid transfer-encoding etc? *)
      if not
           (String.Caseless.equal key "content-length"
           || String.Caseless.equal key "transfer-encoding")
      then (
        Writer.write writer key;
        Writer.write writer ": ";
        Writer.write writer data;
        Writer.write writer "\r\n"))
    headers;
  (match encoding with
  | Http.Transfer.Fixed len ->
    Writer.write writer "content-length: ";
    Writer.write writer (Int64.to_string len);
    Writer.write writer "\r\n"
  | Http.Transfer.Chunked -> Writer.write writer "content-length: chunked\r\n"
  | Http.Transfer.Unknown ->
    (* TODO: This situation shouldn't happen but maybe we should deal with this
       somehow? *)
    ());
  Writer.write writer "\r\n"
;;

let run_server_loop handle_request reader writer =
  let rec loop reader writer handle_request =
    let view = Input_channel.view reader in
    match Parser.parse_request view.buf ~pos:view.pos ~len:view.len with
    | Error Partial ->
      (match%bind Input_channel.refill reader with
      | `Ok -> loop reader writer handle_request
      | `Eof | `Buffer_is_full -> Deferred.unit)
    | Error (Msg msg) ->
      Logger.error "request parser: %s" msg;
      let response = Http.Response.make ~status:`Bad_request () in
      write_response writer (Http.Transfer.Fixed 0L) response;
      Output_channel.flush writer
    | Ok (req, consumed) ->
      Input_channel.consume reader consumed;
      let req_body = Body.Reader.Private.create req reader in
      let%bind res, res_body = handle_request (req, req_body) in
      let keep_alive =
        Http.Request.is_keep_alive req && Http.Response.is_keep_alive res
      in
      write_response writer (Body.Writer.encoding res_body) res;
      let%bind () = Body.Writer.Private.write res_body writer in
      let%bind () = Body.Reader.drain req_body in
      if keep_alive then loop reader writer handle_request else Deferred.unit
  in
  loop reader writer handle_request
;;

let run
    ?(where_to_listen = Tcp.Where_to_listen.of_port 8080)
    ?max_connections
    ?(max_accepts_per_batch = 64)
    ?backlog
    ?socket
    ?initial_buffer_size
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
    ~f:(fun _addr reader writer -> run_server_loop service reader writer)
;;

let run_command ?readme ~summary service =
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
            ~where_to_listen:(Tcp.Where_to_listen.of_port port)
            ~max_accepts_per_batch
            ?max_connections
            ?backlog
            ?initial_buffer_size
            service
        in
        Tcp.Server.close_finished_and_handlers_determined server)
;;
