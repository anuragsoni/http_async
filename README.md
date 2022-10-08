# Http_async

HTTP 1.1 server for async applications.

### Getting Started

This library isn't published to the opam repository yet so it requires pinning to the development version of dependencies.

```
opam pin add -n http_async.dev git+https://github.com/anuragsoni/http_async.git
opam install http_async
```

### Hello World

```ocaml
open! Core
open! Async
open Http_async

let () =
  Command_unix.run
    (Server.run_command ~summary:"Hello world HTTP Server" (fun (_request, _body) ->
       return (Response.create `Ok, Body.Writer.string "Hello World")))
;;
```

### Routing?

Http_async doesn't ship with a router. There are multiple routing libraries available on opam and using `Http_async` with them should be fairly easy. As an example, integration with [ocaml-dispatch](https://github.com/inhabitedtype/ocaml-dispatch) can be done as so:

```ocaml
open! Core
open! Async
open Http_async

let routes =
  let open Dispatch in
  DSL.create
    [ ( "/hello/:name"
      , fun params _rest ->
          let name = List.Assoc.find_exn params ~equal:String.equal "name" in
          return (Response.create `Ok, Body.Writer.string (sprintf "Hello, %s" name)) )
    ; ("/", fun _params _rest -> Response.create `Ok, Body.Writer.string "Hello World")
    ]
;;

let service (request, body) =
  let path = Request.path request in
  match Dispatch.dispatch routes path with
  | Some response -> response
  | None -> return (Response.create `Not_found, Body.Writer.string "Route not found")
;;

let () = Command_unix.run (Server.run_command ~summary:"Hello world HTTP Server" service)
```
