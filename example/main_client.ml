open Core
open Async

let main host () =
  let w = Tcp.Where_to_connect.of_host_and_port host in
  let error_handler _ = assert false in
  let headers =
    Httpaf.Headers.of_list
      [ "User-Agent", "async_http"
      ; "Host", Host_and_port.host host
      ; "Connection", "close"
      ]
  in
  let request = Httpaf.Request.create ~headers `GET "/" in
  Async_http.Client.SSl.connect ~error_handler ~request w
  >>| fun (r, b) ->
  print_endline (Format.asprintf "%a" Httpaf.Response.pp_hum r);
  print_endline b
;;

let () =
  Command.async
    ~summary:"Client test"
    Command.Param.(
      map (anon ("host" %: Command.Param.host_and_port)) ~f:(fun host -> main host))
  |> Command.run
;;
