# async-http

This is very much a Work in progress, and is far from usable. Async based http toolkit built around httpaf.

Server examples:

```ocaml
open Core
open Async

(* Simple handle that responds with a string *)
let handler _req =
  let open Async_http in
  let response = Response.make ~body:(Body.of_string "Hello World") `OK in
  return response
;;

let streaming_handler req =
  let open Async_http in
  let body = req.Request.body in
  (* [length] can be empty if the request transfer encoding was chunked and no
     actual length was provided in the header. *)
  let length, pipe = Body.length body, Body.to_string_pipe body in
  (* We can use any of the async pipe utilities to create a streaming response
     body. *)
  let response_body =
    Pipe.create_reader ~close_on_exception:true (fun writer ->
        Pipe.transfer pipe writer ~f:String.uppercase)
  in
  let response = Response.make ~body:(Body.of_string_pipe ?length response_body) `OK in
  return response
;;
```
