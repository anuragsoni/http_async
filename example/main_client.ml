open Core
open Async

let uri = Uri.of_string "https://httpbin.org/post"

let request_body =
  let make w = Core.Unix.IOVec.of_bigstring (Bigstring.of_string w) in
  Pipe.of_list [ make "foo"; make "bar" ] |> Async_http.Body.of_stream
;;

let main () =
  match%bind Async_http.Client.post ~body:request_body uri with
  | Error err -> Deferred.Result.fail err
  | Ok (_response, body) ->
    let b = Async_http.Body.content body in
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
