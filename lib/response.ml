open Core

type status = Httpaf.Status.t

let sexp_of_status s = sexp_of_string (Format.asprintf "%a" Httpaf.Status.pp_hum s)

type t =
  { status : status
  ; headers : Headers.t
  ; body : Body.t
  }
[@@deriving sexp_of, fields]

let create ?(headers = Headers.empty) ?(body = Body.empty) status =
  { status; headers; body }
;;

let pp fmt t = Sexp.pp fmt (sexp_of_t t)
let pp_hum fmt t = Sexp.pp_hum fmt (sexp_of_t t)

let of_string ?(headers = Headers.empty) ?(status = `OK) body =
  let headers =
    Headers.add_unless_exists
      ~key:"content-length"
      ~value:(Int.to_string (String.length body))
      headers
  in
  create ~headers ~body:(Body.of_string body) status
;;

let of_bigstring ?(headers = Headers.empty) ?(status = `OK) body =
  let headers =
    Headers.add_unless_exists
      ~key:"content-length"
      ~value:(Int.to_string (Bigstring.length body))
      headers
  in
  create ~headers ~body:(Body.of_bigstring body) status
;;
