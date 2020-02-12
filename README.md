# async-http

This is very much a Work in progress, and is far from usable. Async based http toolkit built around httpaf.

### What does it look like?

Server:
```ocaml
open Core
open Async

let request_handler _request =
  let open Async_http in
  Response.of_string "Hello"
;;

let main port ~crt_file ~key_file ~error_handler =
  let where_to_listen =
    Tcp.Where_to_listen.bind_to
      Tcp.Bind_to_address.Localhost
      (Tcp.Bind_to_port.On_port port)
  in
  Async_http.Server.listen_ssl
    ~on_handler_error:`Ignore
    ~request_handler
    ~crt_file
    ~key_file
    ~error_handler
    where_to_listen
  >>= fun _server ->
  Deferred.never ()
;;
```
Client:
```ocaml
open Core
open Async

let main uri () =
  let w = Uri.of_string uri in
  let error_handler _ = assert false in
  Async_http.Client.request ~error_handler ~meth:`GET w
  >>= fun resp ->
  Log.Global.info "Response: %s" (Format.asprintf "%a" Async_http.Response.pp_hum resp);
  let b = Async_http.Body.to_string_stream resp.body in
  Pipe.iter b ~f:(fun w -> return (Log.Global.printf "%s" w))
;;
```
