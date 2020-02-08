open Core
open Async

let run_client r w ~error_handler ~request =
  let read_body finished response body =
    let bs = Bigbuffer.create 0x100 in
    let on_eof () = Ivar.fill finished (response, Bigbuffer.contents bs) in
    let rec on_read buf ~off:pos ~len =
      Bigbuffer.add_bigstring bs (Bigstring.sub_shared buf ~pos ~len);
      Httpaf.Body.schedule_read body ~on_eof ~on_read
    in
    Httpaf.Body.schedule_read body ~on_eof ~on_read
  in
  let resp = Ivar.create () in
  let response_handler r response response_body = read_body r response response_body in
  let body =
    Client0.request ~error_handler ~response_handler:(response_handler resp) request r w
  in
  Httpaf.Body.close_writer body;
  Ivar.read resp
;;

let connect ~error_handler ~request where_to_connect =
  Tcp.(with_connection where_to_connect) (fun _ r w ->
      run_client r w ~request ~error_handler)
;;

module SSl = struct
  open Async_ssl

  let connect ~error_handler ~request where_to_connect =
    Tcp.(connect where_to_connect)
    >>= fun (_, r, w) ->
    let net_to_ssl, ssl_to_net = Io_util.pipes_from_reader_writer r w in
    let app_to_ssl, app_writer = Pipe.create () in
    let app_reader, ssl_to_app = Pipe.create () in
    Ssl.client ~app_to_ssl ~ssl_to_app ~net_to_ssl ~ssl_to_net ()
    >>= function
    | Error error -> Io_util.close_reader_writer r w >>= fun () -> Error.raise error
    | Ok _conn ->
      Io_util.pipes_to_reader_writer app_reader app_writer
      >>= fun (reader, writer) -> run_client reader writer ~request ~error_handler
  ;;
end
