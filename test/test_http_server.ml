open! Core
open! Async
open! Shuttle

let pipe () =
  Unix.pipe (Info.of_string "test shuttle http")
  >>| fun (`Reader reader, `Writer writer) ->
  let a = Input_channel.create reader in
  let b = Output_channel.create writer in
  a, b
;;

let test_post_req_with_fixed_body =
  "POST /hello HTTP/1.1\r\nHost: www.example.com   \r\nContent-Length: 5\r\n\r\nHello\r\n"
;;

let%expect_test "test simple server" =
  let open Async_http in
  let stdout = Lazy.force Writer.stdout in
  let handler request =
    let body = Service.body request in
    let%bind () =
      Pipe.iter_without_pushback body ~f:(fun v -> Writer.write_line stdout v)
    in
    Writer.write_sexp ~hum:true stdout (Service.sexp_of_request request);
    Service.respond_string
      ~headers:[ "content-length", "5"; "connection", "close" ]
      "World"
  in
  let%bind reader, write_to_reader = pipe () in
  let%bind read_from_writer, writer = pipe () in
  let reader_pipe = Input_channel.pipe read_from_writer in
  let finished = Ivar.create () in
  (Async_http.Server.run_server_loop handler reader writer
  >>> fun () -> Ivar.fill finished ());
  Output_channel.write write_to_reader test_post_req_with_fixed_body;
  Output_channel.schedule_flush write_to_reader;
  let%bind () = Ivar.read finished in
  let%bind () =
    [%expect
      {|
    Hello
    (((meth POST) (headers ((Host www.example.com) (Content-Length 5)))
      (version HTTP/1.1) (resource /hello))
     ((encoding (Fixed 5)) (reader <opaque>))) |}]
  in
  let%bind () = Output_channel.close writer in
  let%bind () =
    Pipe.iter_without_pushback reader_pipe ~f:(fun v -> Writer.writef stdout "%S" v)
  in
  [%expect
    {| "HTTP/1.1 200 OK \r\nconnection: close\r\ncontent-length: 5\r\n\r\nWorld" |}]
;;
