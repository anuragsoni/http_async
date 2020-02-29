open Core_kernel

type meth = Httpaf.Method.t

let sexp_of_meth t = sexp_of_string (Httpaf.Method.to_string t)

type t =
  { target : string
  ; state : Univ_map.t
  ; headers : Headers.t
  ; body : Body.t
  ; meth : meth
  }
[@@deriving sexp_of, fields]

let make ?(headers = Headers.empty) ?(body = Body.empty) meth target =
  { target; headers; body; meth; state = Univ_map.empty }
;;
