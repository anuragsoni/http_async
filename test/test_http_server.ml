open! Core
open! Async
open! Shuttle
open Http_async

let default_service _ = return (Response.create `Ok, Body.Writer.string "Hello World")

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

let test_post_req_with_invalid_body_length =
  "POST /hello HTTP/1.1\r\n\
   Host: www.example.com   \r\n\
   Content-Length: 5\r\n\
   Content-Length: 6\r\n\
   \r\n\
   Hello\r\n"
;;

let%expect_test "test simple server" =
  let stdout = Lazy.force Writer.stdout in
  let handler (request, body) =
    let%bind () =
      Pipe.iter_without_pushback (Body.Reader.pipe body) ~f:(fun v ->
        Writer.write_line stdout (Bigstring.to_string v.buf ~pos:v.pos ~len:v.len))
    in
    Writer.write_sexp
      ~hum:true
      stdout
      [%sexp { request : Request.t; body : Body.Reader.t }];
    return
      ( Response.create
          ~headers:(Headers.of_rev_list [ "content-length", "5"; "connection", "close" ])
          `Ok
      , Body.Writer.string "World" )
  in
  let%bind reader, write_to_reader = pipe () in
  let%bind read_from_writer, writer = pipe () in
  let reader_pipe = Input_channel.pipe read_from_writer in
  let finished = Ivar.create () in
  (Server.run_server_loop handler reader writer >>> fun () -> Ivar.fill finished ());
  Output_channel.write write_to_reader test_post_req_with_fixed_body;
  Output_channel.schedule_flush write_to_reader;
  let%bind () = Ivar.read finished in
  [%expect
    {|
    Hello
    ((request
      ((meth POST) (path /hello) (version Http_1_1)
       (headers ((Host www.example.com) (Content-Length 5)))))
     (body (Stream (encoding (Fixed 5)) (reader <opaque>)))) |}];
  let%bind () = Output_channel.close writer in
  let%map () =
    Pipe.iter_without_pushback reader_pipe ~f:(fun v -> Writer.writef stdout "%S" v)
  in
  [%expect {| "HTTP/1.1 200 \r\ncontent-length: 5\r\nconnection: close\r\n\r\nWorld" |}]
;;

let%expect_test "test_default_error_handler" =
  let stdout = Lazy.force Writer.stdout in
  let service _request = failwith "ERROR" in
  let%bind reader, write_to_reader = pipe () in
  let%bind read_from_writer, writer = pipe () in
  let reader_pipe = Input_channel.pipe read_from_writer in
  let finished = Ivar.create () in
  (Server.run_server_loop service reader writer >>> fun () -> Ivar.fill finished ());
  Output_channel.write write_to_reader test_post_req_with_fixed_body;
  Output_channel.schedule_flush write_to_reader;
  let%bind () = Ivar.read finished in
  let%bind () = Output_channel.close writer in
  let%map () =
    Pipe.iter_without_pushback reader_pipe ~f:(fun chunk ->
      Writer.writef stdout "%S" chunk)
  in
  [%expect {| "HTTP/1.1 500 \r\nConnection: close\r\nContent-Length: 0\r\n\r\n" |}]
;;

let%expect_test "test_custom_error_handler" =
  let error_handler ?exn:_ ?request status =
    let body =
      match request with
      | None -> "Something bad happened"
      | Some request ->
        sprintf "Something bad happened in request: %s" (Request.path request)
    in
    return (Response.create status, Body.Writer.string body)
  in
  let stdout = Lazy.force Writer.stdout in
  let service _request = failwith "ERROR" in
  let%bind reader, write_to_reader = pipe () in
  let%bind read_from_writer, writer = pipe () in
  let reader_pipe = Input_channel.pipe read_from_writer in
  let finished = Ivar.create () in
  (Server.run_server_loop ~error_handler service reader writer
  >>> fun () -> Ivar.fill finished ());
  Output_channel.write write_to_reader test_post_req_with_invalid_body_length;
  Output_channel.schedule_flush write_to_reader;
  let%bind () = Ivar.read finished in
  let%bind () = Output_channel.close writer in
  let%map () =
    Pipe.iter_without_pushback reader_pipe ~f:(fun chunk ->
      Writer.writef stdout "%S" chunk)
  in
  [%expect
    {| "HTTP/1.1 400 \r\nContent-Length: 41\r\n\r\nSomething bad happened in request: /hello" |}]
;;

let%expect_test "catches bad request payload" =
  let error_handler ?exn:_ ?request status =
    let body =
      match request with
      | None -> "Something bad happened"
      | Some request ->
        sprintf "Something bad happened in request: %s" (Request.path request)
    in
    return (Response.create status, Body.Writer.string body)
  in
  let stdout = Lazy.force Writer.stdout in
  let service _request = failwith "ERROR" in
  let%bind reader, write_to_reader = pipe () in
  let%bind read_from_writer, writer = pipe () in
  let reader_pipe = Input_channel.pipe read_from_writer in
  let finished = Ivar.create () in
  (Server.run_server_loop ~error_handler service reader writer
  >>> fun () -> Ivar.fill finished ());
  Output_channel.write write_to_reader test_post_req_with_fixed_body;
  Output_channel.schedule_flush write_to_reader;
  let%bind () = Ivar.read finished in
  let%bind () = Output_channel.close writer in
  let%map () =
    Pipe.iter_without_pushback reader_pipe ~f:(fun chunk ->
      Writer.writef stdout "%S" chunk)
  in
  [%expect {| "HTTP/1.1 500 \r\nContent-Length: 22\r\n\r\nSomething bad happened" |}]
;;

let test_post_req_with_chunked_body =
  "POST /hello HTTP/1.1\r\n\
   Host: www.example.com\r\n\
   Transfer-Encoding: chunked\r\n\
   \r\n\
   5\r\n\
   Hello\r\n\
   0\r\n\
   \r\n"
;;

let%expect_test "streaming bodies" =
  let stdout = Lazy.force Writer.stdout in
  let service (_request, body) =
    return (Response.create `Ok, Body.Writer.stream (Body.Reader.pipe body))
  in
  let%bind reader, write_to_reader = pipe () in
  let%bind read_from_writer, writer = pipe () in
  let reader_pipe = Input_channel.pipe read_from_writer in
  let finished = Ivar.create () in
  (Server.run_server_loop service reader writer >>> fun () -> Ivar.fill finished ());
  Output_channel.write write_to_reader test_post_req_with_chunked_body;
  Output_channel.schedule_flush write_to_reader;
  let%bind () = Output_channel.close write_to_reader in
  let%bind () = Ivar.read finished in
  let%bind () = Output_channel.close writer in
  let%map () =
    Pipe.iter_without_pushback reader_pipe ~f:(fun chunk ->
      Writer.writef stdout "%S" chunk)
  in
  [%expect
    {| "HTTP/1.1 200 \r\nTransfer-Encoding: chunked\r\n\r\n5\r\nHello\r\n0\r\n\r\n" |}]
;;

let%expect_test "bad transfer encoding header" =
  let stdout = Lazy.force Writer.stdout in
  let%bind reader, write_to_reader = pipe () in
  let%bind read_from_writer, writer = pipe () in
  let reader_pipe = Input_channel.pipe read_from_writer in
  let finished = Ivar.create () in
  (Server.run_server_loop default_service reader writer
  >>> fun () -> Ivar.fill finished ());
  Output_channel.write
    write_to_reader
    "POST /hello HTTP/1.1\r\n\
     Host: www.example.com   \r\n\
     Transfer-Encoding: foobar\r\n\
     \r\n\
     Hello\r\n";
  Output_channel.schedule_flush write_to_reader;
  let%bind () = Ivar.read finished in
  let%bind () = Output_channel.close writer in
  let%map () =
    Pipe.iter_without_pushback reader_pipe ~f:(fun chunk ->
      Writer.writef stdout "%S" chunk)
  in
  [%expect {| "HTTP/1.1 400 \r\nConnection: close\r\nContent-Length: 0\r\n\r\n" |}]
;;
