open Core
open Async
open Httpaf
open Ppx_log_async
module Unix = Core.Unix
module Logger = Log.Make_global ()

let log = Lazy.force Logger.log

(* Buffer module based on
   https://github.com/inhabitedtype/httpaf/blob/bb5502e0d41bb3916b9e62ff3c0227f728aba850/async/httpaf_async.ml#L7 *)
module Buf = struct
  type t =
    { buf : Bigstring.t
    ; size : int
    ; mutable pos : int
    ; mutable len : int
    }

  let create size = { buf = Bigstring.create size; size; pos = 0; len = 0 }

  let shift t =
    if t.len = 0
    then (
      t.pos <- 0;
      t.len <- 0)
    else if t.pos > 0
    then Bigstring.blit ~src:t.buf ~src_pos:t.pos ~dst:t.buf ~dst_pos:0 ~len:t.len;
    t.pos <- 0
  ;;

  let read t f =
    let num = f t.buf ~pos:t.pos ~len:t.len in
    t.pos <- t.pos + num;
    t.len <- t.len - num;
    if t.len = 0 then t.pos <- 0;
    num
  ;;

  let write t reader =
    shift t;
    let bstr = Bigsubstring.create ~pos:(t.pos + t.len) ~len:(t.size - t.len) t.buf in
    match%map Reader.read_bigsubstring reader bstr with
    | `Eof -> `Eof
    | `Ok n as r ->
      t.len <- t.len + n;
      r
  ;;
end

let write_iovecs writer iovecs =
  match Writer.is_closed writer with
  (* schedule_iovecs will throw if the writer is closed. Checking for the writer status
     here avoids that and allows to report the closed status to httpaf. *)
  | true -> return `Closed
  | false ->
    let iovec_queue = Queue.create ~capacity:(List.length iovecs) () in
    let total_bytes =
      List.fold iovecs ~init:0 ~f:(fun acc { Faraday.buffer; off; len } ->
          Queue.enqueue iovec_queue (Unix.IOVec.of_bigstring buffer ~pos:off ~len);
          acc + len)
    in
    Writer.schedule_iovecs writer iovec_queue;
    (* It is not safe to reuse the underlying bigstrings until the writer is flushed or
       closed. *)
    Writer.flushed writer >>| fun () -> `Ok total_bytes
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
    let buffer = Buf.create config.Httpaf.Config.read_buffer_size in
    let error_handler = error_handler in
    let read_complete = Ivar.create () in
    let write_complete = Ivar.create () in
    let conn = Server_connection.create ~config ~error_handler request_handler in
    let rec reader_thread () =
      match Server_connection.next_read_operation conn with
      | `Read ->
        upon (Buf.write buffer reader) (function
            | `Eof ->
              ignore
                (Buf.read buffer (fun bstr ~pos ~len ->
                     Server_connection.read_eof conn bstr ~off:pos ~len)
                  : int);
              reader_thread ()
            | `Ok _ ->
              ignore
                (Buf.read buffer (fun bstr ~pos ~len ->
                     Server_connection.read conn bstr ~off:pos ~len)
                  : int);
              reader_thread ())
      | `Close ->
        Ivar.fill read_complete ();
        ()
      | `Yield -> Server_connection.yield_reader conn reader_thread
    in
    let rec writer_thread () =
      match Server_connection.next_write_operation conn with
      | `Write iovecs ->
        write_iovecs writer iovecs
        >>> fun result ->
        Server_connection.report_write_result conn result;
        writer_thread ()
      | `Close _ ->
        Ivar.fill write_complete ();
        ()
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
    let read_write_finished =
      Deferred.all_unit [ Ivar.read write_complete; Ivar.read read_complete ]
    in
    read_write_finished
  ;;
end
