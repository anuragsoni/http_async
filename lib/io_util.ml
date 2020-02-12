open Core
open Async

let write_iovecs writer iovecs =
  List.fold_left iovecs ~init:0 ~f:(fun c { Faraday.buffer; off; len } ->
      Writer.write_bigstring writer buffer ~pos:off ~len;
      c + len)
;;

(*  When using this via Tcp.connect, the reader and writer
    share a file descriptor. Closing the reader before closing
    the writer casues the writer to raise an exception because
    of a closed file descriptor. *)
let close_reader_writer reader writer =
  let%bind () = Writer.close ~force_close:(Clock.after (sec 30.)) writer in
  Reader.close reader
;;

(* We do this, instead of using [Reader.pipe] because we don't
   want to close the reader before the writer.
   Implementation is based on: https://github.com/janestreet/async_unix/blob/222b13f24c48262882a09b26e5c0dfed9f41f80b/src/reader0.ml#L1399-L1403 *)
let pipes_from_reader_writer reader writer =
  let rpipe_reader, wpipe_reader = Pipe.create () in
  let writer_pipe = Writer.pipe writer in
  (* [transfer] will transfer content from the reader to the writer
     pipe in chunks. Once EOF is reached or the writer pipe is closed this
     deferred will become determined. *)
  upon (Reader.transfer reader wpipe_reader) (fun () ->
      close_reader_writer reader writer >>> fun () -> Pipe.close wpipe_reader);
  upon (Pipe.closed writer_pipe) (fun () ->
      Deferred.choose
        [ Deferred.choice (Clock.after (sec 30.)) (fun () -> ())
        ; Deferred.choice (Pipe.downstream_flushed writer_pipe) (fun _ -> ())
        ]
      >>> fun () -> don't_wait_for (close_reader_writer reader writer));
  rpipe_reader, writer_pipe
;;

let pipes_to_reader_writer rpipe wpipe =
  let%bind reader = Reader.of_pipe (Info.of_string "ssl_reader") rpipe in
  (* [Reader.of_pipe] does not close the pipe automatically when the reader is finished. *)
  upon (Reader.close_finished reader) (fun () -> Pipe.close_read rpipe);
  let%map writer, _ = Writer.of_pipe (Info.of_string "ssl_writer") wpipe in
  Writer.set_raise_when_consumer_leaves writer false;
  reader, writer
;;
