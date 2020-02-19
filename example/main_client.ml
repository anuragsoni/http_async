open Core
open Async

let where_to_connect =
  Tcp.Where_to_connect.of_host_and_port
    (Host_and_port.create ~host:"httpbin.org" ~port:443)
;;

let main () =
  let open Async_connection in
  let error_handler _ = assert false in
  let print ~on_eof response response_body =
    let open Httpaf in
    match response with
    | { Response.status = `OK; _ } as response ->
      Format.fprintf Format.std_formatter "%a\n%!" Response.pp_hum response;
      let rec on_read bs ~off ~len =
        Bigstringaf.substring ~off ~len bs |> print_string;
        Body.schedule_read response_body ~on_read ~on_eof
      in
      Body.schedule_read response_body ~on_read ~on_eof
    | response ->
      Format.fprintf Format.err_formatter "%a\n%!" Response.pp_hum response;
      Caml.exit 1
  in
  Client.with_connection
    (Client.Secure Client.default_ssl_options)
    where_to_connect
    (fun _addr reader writer ->
      let finished = Ivar.create () in
      let headers = Httpaf.Headers.of_list [ "host", "httpbin.org" ] in
      let request = Httpaf.Request.create ~headers `GET "/get" in
      let response_handler = print ~on_eof:(Ivar.fill finished) in
      let request_body =
        Async_http.Client.request ~response_handler ~error_handler request reader writer
      in
      Httpaf.Body.close_writer request_body;
      Deferred.ok (Ivar.read finished))
;;

let () =
  Command.async_spec ~summary:"Sample client" Command.Spec.empty (fun () ->
      main ()
      >>= function
      | Error e -> Error.raise e
      | Ok () -> Deferred.unit)
  |> Command.run
;;
