open Core_kernel

type status = Httpaf.Status.t

let sexp_of_status t = sexp_of_int (Httpaf.Status.to_code t)

type t =
  { status : status
  ; headers : Headers.t
  ; body : Body.t
  ; state : Univ_map.t
  }
[@@deriving sexp_of]

let make ?(headers = Headers.empty) ?(body = Body.empty) status =
  { status; headers; body; state = Univ_map.empty }
;;
