open! Core
open! Async
open Shuttle_http

type t =
  { name : string
  ; run : Request.t -> Body.Reader.t -> Server.response Deferred.t
  }

let create name run =
  let run request body =
    let%map response, response_body = run request body in
    let headers =
      Http.Header.add_transfer_encoding
        (Http.Response.headers response)
        (Body.Writer.encoding response_body)
    in
    { response with headers }, response_body
  in
  { name; run }
;;

let callback t = t.run

let respond ?(headers = Http.Header.init ()) ?(body = Body.Writer.empty) status =
  let encoding = Body.Writer.encoding body in
  let resp = Http.Response.make ~status ~encoding ~headers () in
  return (resp, body)
;;

let respond_string ?headers ?(status = `OK) body =
  respond ?headers ~body:(Body.Writer.string body) status
;;
