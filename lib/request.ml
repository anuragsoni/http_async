open Core

type meth = Httpaf.Method.t

let sexp_of_meth m = sexp_of_string (Format.asprintf "%a" Httpaf.Method.pp_hum m)
let meth_of_sexp m = Httpaf.Method.of_string (Sexp.to_string m)

type t =
  { target : string
  ; headers : Headers.t
  ; meth : meth
  ; body : Body.t
  }
[@@deriving sexp_of, fields]

let pp fmt t = Sexp.pp fmt (sexp_of_t t)
let pp_hum fmt t = Sexp.pp_hum fmt (sexp_of_t t)

let to_httpaf_request t =
  Httpaf.Request.create ~headers:(Httpaf.Headers.of_rev_list t.headers) t.meth t.target
;;
