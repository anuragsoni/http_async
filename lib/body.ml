open Core
open Async

type t =
  [ `Empty
  | `String of string
  | `Bigstring of (Bigstring.t Faraday.iovec[@sexp.opaque])
  | `Stream of (Bigstring.t Faraday.iovec Pipe.Reader.t[@sexp.opaque])
  ]
[@@deriving sexp_of]

let empty = `Empty
let of_string s = `String s

let iovec_to_string { Faraday.buffer; off; len } =
  Bigstring.to_string ~pos:off ~len buffer
;;

let to_string = function
  | `Empty -> return ""
  | `String s -> return s
  | `Bigstring b -> return (iovec_to_string b)
  | `Stream s -> Pipe.to_list s >>| fun x -> String.concat (List.map ~f:iovec_to_string x)
;;

let to_string_stream = function
  | `Empty -> Pipe.of_list []
  | `String s -> Pipe.singleton s
  | `Bigstring b -> Pipe.singleton (iovec_to_string b)
  | `Stream s -> Pipe.map s ~f:(fun b -> iovec_to_string b)
;;

let of_stream reader = `Stream reader

let drain = function
  | `Stream s -> Pipe.drain s
  | _ -> return ()
;;

let pp fmt t = Sexp.pp fmt (sexp_of_t t)
let pp_hum fmt t = Sexp.pp_hum fmt (sexp_of_t t)
