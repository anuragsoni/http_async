open Core
open Async
module Unix = Core.Unix

type iovec = Bigstring.t Unix.IOVec.t [@@deriving sexp_of]

let iovec_to_string { Unix.IOVec.buf; pos; len } = Bigstring.to_string buf ~pos ~len
let iovec_of_string s = Unix.IOVec.of_bigstring (Bigstring.of_string s)

type content = iovec Pipe.Reader.t [@@deriving sexp_of]

type t =
  { length : Int64.t option
  ; content : content
  }
[@@deriving sexp_of, fields]

let drain { content; _ } = Pipe.drain content

let to_string { content; _ } =
  let string_pipe = Pipe.map ~f:iovec_to_string content in
  let%map segments = Pipe.to_list string_pipe in
  String.concat segments
;;

let of_string s =
  { content = Pipe.singleton (Unix.IOVec.of_bigstring (Bigstring.of_string s))
  ; length = Some (Int64.of_int (String.length s))
  }
;;

let of_bigstring b =
  { content = Pipe.singleton (Unix.IOVec.of_bigstring b)
  ; length = Some (Int64.of_int (Bigstring.length b))
  }
;;

let of_stream ?length s = { content = s; length }
let empty = { content = Pipe.of_list []; length = Some 0L }

let read_httpaf_body ?length finished body =
  let on_eof' () =
    Httpaf.Body.close_reader body;
    Ivar.fill finished ();
    return @@ `Finished ()
  in
  let on_read' writer b ~off ~len =
    let module Unix = Core.Unix in
    let%map () = Pipe.write_if_open writer (Unix.IOVec.of_bigstring ~pos:off ~len b) in
    `Repeat ()
  in
  let b =
    (* Async recommends choosing false for [close_on_exception]. In a normal flow,
       closing the write end of the pipe will indicate that the writer finished successfully. *)
    Pipe.create_reader ~close_on_exception:false (fun writer ->
        (* [create_reader] will automatically close the writer end, when this Deferred becomes
           determined. We loop here so we can process a chain of Httpaf read events. *)
        Deferred.repeat_until_finished () (fun () ->
            let next_iter = Ivar.create () in
            let on_eof () =
              don't_wait_for (on_eof' () >>| fun n -> Ivar.fill next_iter n)
            in
            let on_read buffer ~off ~len =
              don't_wait_for
                (on_read' writer buffer ~off ~len >>| fun n -> Ivar.fill next_iter n)
            in
            Httpaf.Body.schedule_read body ~on_eof ~on_read;
            Ivar.read next_iter))
  in
  of_stream ?length b
;;
