open Core
open Async
open Httpaf

let create_connection_handler
    ?(config = Config.default)
    ~request_handler
    ~error_handler
    addr
    reader
    writer
  =
  let request_handler = request_handler addr in
  let error_handler error_ivar = error_handler addr error_ivar in
  let read_complete = Ivar.create () in
  let write_complete = Ivar.create () in
  let error_ivar = Ivar.create () in
  let conn =
    Server_connection.create
      ~config
      ~error_handler:(error_handler error_ivar)
      request_handler
  in
  let read_eof conn buf =
    ignore
      (Server_connection.read_eof
         conn
         (Bigstring.of_string buf)
         ~off:0
         ~len:(String.length buf)
        : int)
  in
  let rec reader_thread () =
    match Server_connection.next_read_operation conn with
    | `Read ->
      Reader.read_one_chunk_at_a_time reader ~handle_chunk:(fun buf ~pos ~len ->
          let c = Server_connection.read conn buf ~off:pos ~len in
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
      Ivar.fill read_complete ();
      ()
    | `Yield -> Server_connection.yield_reader conn reader_thread
  in
  let rec writer_thread () =
    match Server_connection.next_write_operation conn with
    | `Write iovecs ->
      let c = Io_util.write_iovecs writer iovecs in
      Server_connection.report_write_result conn (`Ok c);
      writer_thread ()
    | `Close _ ->
      Ivar.fill write_complete ();
      ()
    | `Yield -> Server_connection.yield_writer conn writer_thread
  in
  let monitor = Monitor.create () in
  Scheduler.within ~monitor reader_thread;
  Scheduler.within ~monitor writer_thread;
  Monitor.detach_and_iter_errors monitor ~f:(fun exn ->
      Server_connection.report_exn conn exn);
  let ps = Deferred.all_unit [ Ivar.read write_complete; Ivar.read read_complete ] in
  (* TODO: fix this. Should not need to wait a second for this *)
  Deferred.any_unit [ ps; (Ivar.read error_ivar >>= fun () -> after Time.Span.second) ]
;;
