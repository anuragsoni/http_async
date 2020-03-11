# async-http

This is very much a Work in progress, and is far from usable. Async based http toolkit built around httpaf.

Server examples:

```ocaml
open Core
open Async

(* The pattern matching on path segments is done to keep the example simple.
   In a real application one can use a routing library like [ocaml-dispatch]. *)
let handler req =
  let open Async_http in
  match String.split ~on:'/' req.Request.target with
  | [ ""; "hello" ] -> Response.of_string "Hello World!"
  | [ ""; "bigstring" ] -> Response.of_bigstring Test_data.text
  | [ ""; "greet"; name ] when String.length name > 0 ->
    Response.of_string (sprintf "Hello, %s" name)
  | [ ""; "file" ] -> Response.of_file "./test/sample.html"
  | [ ""; "stream" ] ->
    (* In a real application this is where one can check if the client request
       is using the appropriate HTTP method. *)
    let body = req.Request.body in
    (* [length] can be empty if the request transfer encoding was chunked and no
       actual length was provided in the header. *)
    let length, pipe = Body.length body, Body.to_pipe body in
    (* We can use any of the async pipe utilities to create a streaming response
       body. *)
    let response_body =
      Pipe.create_reader ~close_on_exception:true (fun writer ->
          Pipe.transfer pipe writer ~f:String.uppercase)
    in
    return (Response.make ~body:(Body.of_pipe ?length response_body) `OK)
  | _ -> Response.of_string ~status:`Not_found "Route not found"
;;
```
