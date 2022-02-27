# Async_http

Simple HTTP server for async applications. This library implements HTTP 1.1 specification including support for pipelined connections.

### Hello World

```ocaml
open! Core
open! Async
open Async_http

let () =
  Command_unix.run
    (Server.run_command ~summary:"Hello world HTTP Server" (fun _request ->
         Service.respond_string "Hello World"))
;;
```