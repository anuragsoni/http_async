# async-http

Work in progress async based http toolkit.
The project aims to complement [Async](https://opensource.janestreet.com/async/) and [httpaf](https://github.com/inhabitedtype/httpaf) by providing utilities to help setup async HTTP servers and clients. The project includes the following libraries:

1. Async_connection: A library that aims to provide a uniform interface to create regular TCP connections, and connections encrypted via async_ssl. The library exposes functions from the Tcp module from Async that work with regular Async readers and writers.

2. Async_http: A library that provides a simpler way to create `request -> response Deferred.t` style request handlers. It is built on top of httpaf, and enhances the request/response types with a `Async_kernel.Pipe.t` aware Body type that can be used for streaming, simpler request and error handlers and a request/response state modelled using a heterogenous map. The handlers return `Deferred.t` so they can be combined using regular async combinators. Async_http can be used without Async_connection by plugging it into anything that works with async readers and writers.

### Install

```sh
opam pin add async-connection.dev git+https://github.com/anuragsoni/async-http.git
opam pin add async-http.dev git+https://github.com/anuragsoni/async-http.git
```

### Examples

Server:

```ocaml
open Core
open Async

(* The pattern matching on path segments is done to keep the example simple.
   In a real application one can use a routing library like [ocaml-dispatch]. *)
let handler req =
  let open Async_http in
  match String.split ~on:'/' req.Request.target with
  | [ ""; "hello" ] -> Response.create (Body.of_string "Hello")
  | [ ""; "bigstring" ] -> Response.create (Body.of_bigstring Test_data.text)
  | [ ""; "greet"; name ] when String.length name > 0 ->
    Response.create (Body.of_string (sprintf "Hello, %s" name))
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
    Response.create (Body.of_pipe ?length response_body)
  | _ -> Response.create ~status:`Not_found (Body.of_string "Route not found")
;;
```

Client:

```ocaml
open Core
open Async
open Async_http

let uri = Uri.of_string "https://httpbin.org/post"

let post =
  let open Async_http in
  match%bind
    Async_http.Client.post
      ~body:(Async_http.Body.of_string "Hello World")
      uri
  with
  | Error err ->
    Log.Global.error "Error happened";
    Deferred.Result.fail err
  | Ok { Response.body; _ } ->
    Async_http.Body.to_pipe body
    |> Pipe.iter_without_pushback ~continue_on_error:true ~f:(fun w ->
           Log.Global.printf "%s" w)
    >>= fun () -> Deferred.Or_error.ok_unit
;;
```
