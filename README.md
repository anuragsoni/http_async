# async-http

This is very much a Work in progress, and is far from usable. Async based http toolkit built around httpaf.

Homepage: https://gitlab.com/anuragsoni/async-http

### What does it look like?

Server:
```ocaml
open Core
open Async

module R = struct
  open Routes
  open Async_http

  let not_found = Response.of_string ~status:`Not_found "Not found"

  let routes =
    one_of
      [ (nil @--> fun _req -> Response.of_string "Hello World")
      ; ((s "greet" / str /? nil)
        @--> fun a req ->
        let message = "Hello, " ^ a in
        Log.Global.info_s ([%sexp_of: Request.t] req);
        Response.of_string message)
      ]
  ;;
end

let request_handler req =
  let open Async_http in
  match Routes.match' ~target:req.Request.target R.routes with
  | None -> R.not_found
  | Some r -> r req
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
