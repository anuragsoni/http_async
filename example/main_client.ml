open Core
open Async

let main uri () =
  let w = Uri.of_string uri in
  let error_handler _ = assert false in
  Async_http.Client.request ~ca_file:"./certs/localhost.crt" ~error_handler ~meth:`GET w
  >>| fun (r, b) ->
  print_endline (Format.asprintf "%a" Httpaf.Response.pp_hum r);
  print_endline b
;;

let () =
  Command.async
    ~summary:"Client test"
    Command.Param.(map (anon ("uri" %: string)) ~f:(fun uri -> main uri))
  |> Command.run
;;
