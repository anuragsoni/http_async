open! Core_kernel

type t = Http.Response.t

let sexp_of_t { Http.Response.headers; version; status; _ } =
  [%sexp
    { headers = (Http.Header.to_list headers : (string, string) List.Assoc.t)
    ; version = (Http.Version.to_string version : string)
    ; status = (Http.Status.to_string status : string)
    ; reason_phrase =
        (Http.Status.reason_phrase_of_code (Http.Status.to_int status) : string)
    }]
;;

let headers t = Http.Response.headers t
let version t = Http.Response.version t
let status t = Http.Response.status t
