open Core
open Async
module Unix = Core.Unix

type iovec = Bigstring.t Unix.IOVec.t [@@deriving sexp_of]

let iovec_to_string { Unix.IOVec.buf; pos; len } = Bigstring.to_string buf ~pos ~len
let iovec_of_string s = Unix.IOVec.of_bigstring (Bigstring.of_string s)

type t =
  | Empty
  | String of string
  | Bigstring of iovec
  | Stream of iovec Pipe.Reader.t
[@@deriving sexp_of, variants]

let drain = function
  | Stream s -> Pipe.drain s
  | _ -> Deferred.unit
;;

let to_string = function
  | Empty -> return ""
  | String s -> return s
  | Bigstring iovec -> return @@ iovec_to_string iovec
  | Stream s ->
    let string_pipe = Pipe.map ~f:iovec_to_string s in
    let%map segments = Pipe.to_list string_pipe in
    String.concat segments
;;

let to_pipe = function
  | Empty -> Pipe.of_list []
  | String s -> Pipe.singleton (iovec_of_string s)
  | Bigstring iovec -> Pipe.singleton iovec
  | Stream s -> s
;;
