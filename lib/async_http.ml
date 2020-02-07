open Core
open Async

module Server = struct
  let listen
      ?buffer_age_limit
      ?max_connections
      ?max_accepts_per_batch
      ?backlog
      ?socket
      ~on_handler_error
      ~request_handler
      ~error_handler
      where_to_listen
    =
    Tcp.(
      Server.create
        ?buffer_age_limit
        ?max_connections
        ?max_accepts_per_batch
        ?backlog
        ?socket
        ~on_handler_error
        where_to_listen)
      (Server0.create_connection_handler ~request_handler ~error_handler)
  ;;
end

module Client = struct
  let connect ~error_handler ~request where_to_connect =
    let read_body finished response body =
      let bs = Bigbuffer.create 0x100 in
      let on_eof () = Ivar.fill finished (response, Bigbuffer.contents bs) in
      let rec on_read buf ~off:pos ~len =
        Bigbuffer.add_bigstring bs (Bigstring.sub_shared buf ~pos ~len);
        Httpaf.Body.schedule_read body ~on_eof ~on_read
      in
      Httpaf.Body.schedule_read body ~on_eof ~on_read
    in
    Tcp.(with_connection where_to_connect) (fun _ r w ->
        let resp = Ivar.create () in
        let response_handler r response response_body =
          read_body r response response_body
        in
        let body =
          Client0.request
            ~error_handler
            ~response_handler:(response_handler resp)
            request
            r
            w
        in
        Httpaf.Body.close_writer body;
        Ivar.read resp)
  ;;
end

module SSl = struct
  open Async_ssl

  let close_reader_writer reader writer =
    Writer.close ~force_close:(Clock.after (sec 30.)) writer
    >>= fun () -> Reader.close reader
  ;;

  let pipes_from_reader_writer reader writer =
    let rpipe_reader, wpipe_reader = Pipe.create () in
    let writer_pipe = Writer.pipe writer in
    upon (Reader.transfer reader wpipe_reader) (fun () ->
        close_reader_writer reader writer >>> fun () -> Pipe.close wpipe_reader);
    upon (Pipe.closed writer_pipe) (fun () ->
        Deferred.choose
          [ Deferred.choice (Clock.after (sec 30.)) (fun () -> ())
          ; Deferred.choice (Pipe.downstream_flushed writer_pipe) (fun _ -> ())
          ]
        >>> fun () -> don't_wait_for (close_reader_writer reader writer));
    rpipe_reader, writer_pipe
  ;;

  let pipes_to_reader_writer rpipe wpipe =
    Reader.of_pipe (Info.of_string "ssl_reader") rpipe
    >>= fun reader ->
    upon (Reader.close_finished reader) (fun () -> Pipe.close_read rpipe);
    Writer.of_pipe (Info.of_string "ssl_writer") wpipe
    >>| fun (writer, _) ->
    Writer.set_raise_when_consumer_leaves writer false;
    reader, writer
  ;;

  module Client = struct
    let connect ~error_handler ~request where_to_connect =
      let read_body finished response body =
        let bs = Bigbuffer.create 0x100 in
        let on_eof () = Ivar.fill finished (response, Bigbuffer.contents bs) in
        let rec on_read buf ~off:pos ~len =
          Bigbuffer.add_bigstring bs (Bigstring.sub_shared buf ~pos ~len);
          Httpaf.Body.schedule_read body ~on_eof ~on_read
        in
        Httpaf.Body.schedule_read body ~on_eof ~on_read
      in
      Tcp.(connect where_to_connect)
      >>= fun (_, r, w) ->
      let net_to_ssl, ssl_to_net = pipes_from_reader_writer r w in
      let app_to_ssl, app_writer = Pipe.create () in
      let app_reader, ssl_to_app = Pipe.create () in
      Ssl.client ~app_to_ssl ~ssl_to_app ~net_to_ssl ~ssl_to_net ()
      >>= function
      | Error error -> close_reader_writer r w >>= fun () -> Error.raise error
      | Ok _conn ->
        pipes_to_reader_writer app_reader app_writer
        >>= fun (reader, writer) ->
        let resp = Ivar.create () in
        let response_handler r response response_body =
          read_body r response response_body
        in
        let body =
          Client0.request
            ~error_handler
            ~response_handler:(response_handler resp)
            request
            reader
            writer
        in
        Httpaf.Body.close_writer body;
        Ivar.read resp
    ;;
  end
end
