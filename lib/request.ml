open! Core_kernel
open Shuttle_http

type request = Http.Request.t

let sexp_of_request { Http.Request.headers; meth; scheme; resource; version; _ } =
  [%sexp
    { headers = (Http.Header.to_list headers : (string, string) List.Assoc.t)
    ; meth = (Http.Method.to_string meth : string)
    ; scheme : string option
    ; resource : string
    ; version = (Http.Version.to_string version : string)
    }]
;;

type t =
  { request : request
  ; body : Body.Reader.t
  }
[@@deriving sexp_of]

let create request body = { request; body }
let headers t = Http.Request.headers t.request
let meth t = Http.Request.meth t.request
let resource t = Http.Request.resource t.request
let body t = t.body
