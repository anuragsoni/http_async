open Core
open Async

let uri = Uri.of_string "https://httpbin.org/post"
let request_body () = Pipe.singleton "Hello"

let main () =
  let open Async_http in
  match%bind
    Async_http.Client.post
      ~body:(Async_http.Body.of_pipe ~length:5L (request_body ()))
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

let () =
  Command.async_spec ~summary:"Sample client" Command.Spec.empty (fun () ->
      main ()
      >>= function
      | Error e -> Error.raise e
      | Ok () -> after (Time.Span.of_sec 1.) >>= fun () -> Deferred.unit)
  |> Command.run
;;
