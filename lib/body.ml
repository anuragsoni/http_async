open Core
open Async
module Unix = Core.Unix

type t =
  [ `Empty
  | `String of string
  | `Bigstring of Bigstring.t
  | `Stream of Bigstring.t Unix.IOVec.t Pipe.Reader.t
  ]
[@@deriving sexp_of]

let empty = `Empty
let of_string s = `String s
let of_bigstring b = `Bigstring b
let iovec_to_string { Unix.IOVec.buf; pos; len } = Bigstring.to_string ~pos ~len buf

let to_string = function
  | `Empty -> return ""
  | `String s -> return s
  | `Bigstring b -> return (Bigstring.to_string b)
  | `Stream s -> Pipe.to_list s >>| fun x -> String.concat (List.map ~f:iovec_to_string x)
;;

let to_string_stream = function
  | `Empty -> Pipe.of_list []
  | `String s -> Pipe.singleton s
  | `Bigstring b -> Pipe.singleton (Bigstring.to_string b)
  | `Stream s -> Pipe.map s ~f:iovec_to_string
;;

let of_stream reader = `Stream reader

let drain = function
  | `Stream s -> Pipe.drain s
  | _ -> return ()
;;

let pp fmt t = Sexp.pp fmt (sexp_of_t t)
let pp_hum fmt t = Sexp.pp_hum fmt (sexp_of_t t)
