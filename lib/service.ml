open! Core
open! Async

type t =
  { name : string
  ; run : Request.t -> Response.t Deferred.t
  }

let create name run =
  let run request =
    let%map response, response_body = run request in
    let headers =
      Http.Header.add_transfer_encoding
        (Http.Response.headers response)
        (Shuttle_http.Body.Writer.encoding response_body)
    in
    { response with headers }, response_body
  in
  { name; run }
;;

let callback t request body = t.run (Request.create request body)
