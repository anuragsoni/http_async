open! Core_kernel

type t = Http.Request.t

let sexp_of_t { Http.Request.headers; meth; scheme; resource; version; _ } =
  [%sexp
    { headers = (Http.Header.to_list headers : (string, string) List.Assoc.t)
    ; meth = (Http.Method.to_string meth : string)
    ; scheme : string option
    ; resource : string
    ; version = (Http.Version.to_string version : string)
    }]
;;

let headers t = Http.Request.headers t
let meth t = Http.Request.meth t
let scheme t = Http.Request.scheme t
let resource t = Http.Request.resource t
