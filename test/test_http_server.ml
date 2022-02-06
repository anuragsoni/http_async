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
  let handler (req, body) =
    let%bind () =
      Pipe.iter_without_pushback (Body.Reader.pipe body) ~f:(fun v ->
          Writer.write_line stdout v)
    in
    Writer.write_sexp
      ~hum:true
      stdout
      [%sexp
        { resource = (Http.Request.resource req : string)
        ; headers =
            (Http.Header.to_list (Http.Request.headers req) : (string * string) list)
        ; version = (Http.Version.to_string (Http.Request.version req) : string)
        }];
    return
      ( Http.Response.make
          ~headers:(Http.Header.of_list [ "content-length", "5"; "connection", "close" ])
          ()
      , Body.Writer.string "World" )
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
    ((resource /hello) (headers ((Host www.example.com) (Content-Length 5)))
     (version HTTP/1.1)) |}]
  in
  let%bind () = Output_channel.close writer in
  let%bind () =
    Pipe.iter_without_pushback reader_pipe ~f:(fun v -> Writer.writef stdout "%S" v)
  in
  [%expect
    {| "HTTP/1.1 200 OK \r\nconnection: close\r\ncontent-length: 5\r\n\r\nWorld" |}]
;;
