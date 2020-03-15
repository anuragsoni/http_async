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
