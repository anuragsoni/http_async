open Core
open Async

let main uri () =
  let w = Uri.of_string uri in
  let error_handler _ = assert false in
  Async_http.Client.request ~error_handler ~meth:`GET w
  >>= fun (r, b) ->
  Log.Global.info "Response: %s" (Format.asprintf "%a" Httpaf.Response.pp_hum r);
  let b = Async_http.Body.to_string_stream b in
  Log.Global.info "Is stream closed: %b\n" (Pipe.is_closed b);
  Pipe.iter b ~f:(fun w -> return (Log.Global.printf "%s" w))
  >>| fun () -> Log.Global.info "Is stream closed: %b\n" (Pipe.is_closed b)
;;

let () =
  Command.async
    ~summary:"Client test"
    Command.Param.(map (anon ("uri" %: string)) ~f:(fun uri -> main uri))
  |> Command.run
;;
