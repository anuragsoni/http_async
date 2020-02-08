open Core
open Async
open Httpaf

let request ?config ~response_handler ~error_handler request reader writer =
  let request_body, conn =
    Client_connection.request ?config ~error_handler ~response_handler request
  in
  let read_finished = Ivar.create () in
  let write_finished = Ivar.create () in
  let read_eof conn buf =
    ignore
      (Client_connection.read_eof
         conn
         (Bigstring.of_string buf)
         ~off:0
         ~len:(String.length buf)
        : int)
  in
  let rec reader_thread () =
    match Client_connection.next_read_operation conn with
    | `Read ->
      Reader.read_one_chunk_at_a_time reader ~handle_chunk:(fun buf ~pos ~len ->
          let c = Client_connection.read conn buf ~off:pos ~len in
          return (`Consumed (c, `Need_unknown)))
      >>> (function
      | `Stopped () -> assert false
      | `Eof ->
        read_eof conn "";
        reader_thread ()
      | `Eof_with_unconsumed_data buf ->
        read_eof conn buf;
        reader_thread ())
    | `Close ->
      Ivar.fill read_finished ();
      ()
  in
  let rec writer_thread () =
    match Client_connection.next_write_operation conn with
    | `Write iovecs ->
      let c = Io_util.write_iovecs writer iovecs in
      Client_connection.report_write_result conn (`Ok c);
      writer_thread ()
    | `Yield -> Client_connection.yield_writer conn writer_thread
    | `Close _ ->
      Ivar.fill write_finished ();
      ()
  in
  let monitor = Monitor.create () in
  Scheduler.within ~monitor reader_thread;
  Scheduler.within ~monitor writer_thread;
  Monitor.detach_and_iter_errors monitor ~f:(fun exn ->
      Client_connection.report_exn conn exn);
  don't_wait_for (Deferred.all_unit [ Ivar.read read_finished; Ivar.read write_finished ]);
  request_body
;;
