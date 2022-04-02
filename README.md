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
    (Server.run_command ~summary:"Hello world HTTP Server" (fun request ->
         let%bind () =
           Pipe.iter_without_pushback
             ~f:(fun chunk -> Log.Global.info "%s" chunk)
             (Service.body request)
         in
         Service.respond_string "Hello World"))
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
          Service.respond_string (sprintf "Hello, %s" name) )
    ; ("/", fun _params _rest -> Service.respond_string "Hello World")
    ]
;;

let service request =
  let path = Service.resource request in
  match Dispatch.dispatch routes path with
  | Some response -> response
  | None -> Service.respond_string ~status:`Not_found "Route not found"
;;

let () = Command_unix.run (Server.run_command ~summary:"Hello world HTTP Server" service)
```
