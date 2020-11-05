open Core
open Async
open Httpaf
open Ppx_log_async
module Logger = Log.Make_global ()

let log = Lazy.force Logger.log

let write_iovecs writer iovecs =
  match Writer.is_closed writer with
  (* schedule_iovecs will throw if the writer is closed. Checking for the writer status
     here avoids that and allows to report the closed status to httpaf. *)
  | true -> `Closed
  | false ->
    let total_bytes =
      List.fold_left iovecs ~init:0 ~f:(fun acc { Faraday.buffer; off; len } ->
          Writer.write_bigstring writer buffer ~pos:off ~len;
          acc + len)
    in
    `Ok total_bytes
;;

module Server = struct
  let default_error_handler ?request:_ error start_response =
    let message =
      match error with
      | `Exn e ->
        [%log.error log (e : Exn.t)];
        Status.default_reason_phrase `Internal_server_error
      | (#Status.server_error | #Status.client_error) as error ->
        Status.default_reason_phrase error
    in
    let len = Int.to_string (String.length message) in
    let headers = Headers.of_list [ "content-length", len ] in
    let body = start_response headers in
    Body.write_string body message;
    Body.close_writer body
  ;;

  let create_connection_handler
      ?(config = Httpaf.Config.default)
      ?(error_handler = default_error_handler)
      ~request_handler
      reader
      writer
    =
    let error_handler = error_handler in
    let read_complete = Ivar.create () in
    let write_complete = Ivar.create () in
    let conn = Server_connection.create ~config ~error_handler request_handler in
    let rec reader_thread () =
      match Server_connection.next_read_operation conn with
      | `Read ->
        Reader.read_one_chunk_at_a_time reader ~handle_chunk:(fun buf ~pos ~len ->
            let consumed = Server_connection.read conn buf ~off:pos ~len in
            return (`Consumed (consumed, `Need_unknown)))
        >>> (function
        | `Eof ->
          ignore (Server_connection.read_eof conn Bigstringaf.empty ~off:0 ~len:0 : int);
          reader_thread ()
        | `Eof_with_unconsumed_data buf ->
          let buf = Bigstring.of_string buf in
          ignore
            (Server_connection.read_eof conn buf ~off:0 ~len:(Bigstring.length buf) : int);
          reader_thread ()
        | `Stopped () -> assert false)
      | `Close -> Ivar.fill read_complete ()
      | `Yield -> Server_connection.yield_reader conn reader_thread
    in
    let rec writer_thread () =
      match Server_connection.next_write_operation conn with
      | `Write iovecs ->
        let result = write_iovecs writer iovecs in
        Server_connection.report_write_result conn result;
        writer_thread ()
      | `Close _ -> Ivar.fill write_complete ()
      | `Yield -> Server_connection.yield_writer conn writer_thread
    in
    let monitor = Monitor.create ~here:[%here] ~name:"AsyncHttpServer" () in
    Monitor.detach_and_iter_errors monitor ~f:(fun e ->
        (* TODO: verify that this doesn't cause any issues. In situations where the
           exception happens before reader is finished, we want to "fill" the reader ivar.
           We use [Async_unix.Tcp.create] which expects the deferred to be fulfilled to
           close the reader/writer pair. *)
        Ivar.fill_if_empty read_complete ();
        Server_connection.report_exn conn e);
    Scheduler.within ~monitor reader_thread;
    Scheduler.within ~monitor writer_thread;
    Deferred.all_unit [ Ivar.read write_complete; Ivar.read read_complete ]
  ;;
end
