open Core
open Async_kernel

type content =
  | Empty
  | String of string
  | Bigstring of bigstring
  | Stream of string Pipe.Reader.t
[@@deriving sexp_of]

type t =
  { length : Int64.t option
  ; content : content
  }
[@@deriving sexp_of, fields]

let drain { content; _ } =
  match content with
  | Stream content -> Pipe.drain content
  | _ -> Deferred.unit
;;

let to_string { content; _ } =
  match content with
  | Stream content ->
    let%map segments = Pipe.to_list content in
    String.concat segments
  | String s -> return s
  | Bigstring b -> return (Bigstring.to_string b)
  | Empty -> return ""
;;

let to_pipe { content; _ } =
  match content with
  | Stream content -> content
  | String s -> Pipe.singleton s
  | Bigstring b -> Pipe.singleton (Bigstring.to_string b)
  | Empty -> Pipe.of_list []
;;

let of_string s = { content = String s; length = Some (Int64.of_int (String.length s)) }

let of_bigstring b =
  { content = Bigstring b; length = Some (Int64.of_int (Bigstring.length b)) }
;;

let of_pipe ?length s = { content = Stream s; length }
let empty = { content = Empty; length = Some 0L }

let read_httpaf_body ?length on_finish body =
  let r, w = Pipe.create () in
  let on_eof () =
    on_finish ();
    Pipe.close w
  in
  let rec on_read b ~off ~len =
    Pipe.write_without_pushback_if_open w (Bigstringaf.substring b ~off ~len);
    Httpaf.Body.schedule_read body ~on_eof ~on_read
  in
  Httpaf.Body.schedule_read body ~on_eof ~on_read;
  of_pipe ?length r
;;
