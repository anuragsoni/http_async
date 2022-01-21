open Core
open Shuttle_http

let sexp_of_response { Http.Response.headers; version; status; _ } =
  [%sexp
    { headers = (Http.Header.to_list headers : (string, string) List.Assoc.t)
    ; version = (Http.Version.to_string version : string)
    ; status = (Http.Status.to_string status : string)
    ; reason_phrase =
        (Http.Status.reason_phrase_of_code (Http.Status.to_int status) : string)
    }]
;;

type t = Shuttle_http.Server.response

let sexp_of_t (response, body) =
  [%sexp { response : response; body : Shuttle_http.Body.Writer.t }]
;;

let headers (t, _) = Http.Response.headers t
let status (t, _) = Http.Response.status t
let body (_, body) = body

let respond ?(headers = Http.Header.init ()) ?(body = Body.Writer.empty) status =
  let encoding = Body.Writer.encoding body in
  let resp = Http.Response.make ~status ~encoding ~headers () in
  resp, body
;;

let string ?headers ?(status = `OK) body =
  respond ?headers ~body:(Body.Writer.string body) status
;;
