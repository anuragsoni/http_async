open Core
open Async

let uri = Uri.of_string "https://www.gutenberg.org/files/1661/1661-0.txt"

let main () =
  match%bind Async_http.Client.get uri with
  | Error err -> Deferred.Result.fail err
  | Ok (_response, body) ->
    let b = Async_http.Body.to_pipe body in
    Pipe.iter_without_pushback b ~f:(fun { Core.Unix.IOVec.buf; pos; len } ->
        let word = Bigstring.to_string buf ~pos ~len in
        print_endline word)
    |> Deferred.ok
;;

let () =
  Command.async_spec ~summary:"Sample client" Command.Spec.empty (fun () ->
      main ()
      >>= function
      | Error e -> Error.raise e
      | Ok () -> after (Time.Span.of_sec 1.) >>= fun () -> Deferred.unit)
  |> Command.run
;;
