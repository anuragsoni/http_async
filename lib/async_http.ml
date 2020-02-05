open Core
open Async

module Httpaf_async = struct
  module Buffer : sig
    type t

    val create : int -> t
    val get : t -> f:(Bigstring.t -> off:int -> len:int -> int) -> int
    val put : t -> f:(Bigstring.t -> off:int -> len:int -> int) -> int
  end = struct
    type t =
      { buffer : Bigstring.t
      ; mutable off : int
      ; mutable len : int
      }

    let create size =
      let buffer = Bigstring.create size in
      { buffer; off = 0; len = 0 }
    ;;

    let compress t =
      if t.len = 0
      then (
        t.off <- 0;
        t.len <- 0)
      else if t.off > 0
      then (
        Bigstring.blit ~src:t.buffer ~src_pos:t.off ~dst:t.buffer ~dst_pos:0 ~len:t.len;
        t.off <- 0)
    ;;

    let get t ~f =
      let n = f t.buffer ~off:t.off ~len:t.len in
      t.off <- t.off + n;
      t.len <- t.len - n;
      if t.len = 0 then t.off <- 0;
      n
    ;;

    let put t ~f =
      compress t;
      let n = f t.buffer ~off:(t.off + t.len) ~len:(Bigstring.length t.buffer - t.len) in
      t.len <- t.len + n;
      n
    ;;
  end

  let read fd buffer =
    let badfd fd = failwithf "read got back fd: %s" (Fd.to_string fd) () in
    let rec finish fd buffer result =
      let open Unix.Error in
      match result with
      | `Already_closed | `Ok 0 -> return `Eof
      | `Ok n -> return (`Ok n)
      | `Error (Unix.Unix_error ((EWOULDBLOCK | EAGAIN), _, _)) ->
        Fd.ready_to fd `Read
        >>= (function
        | `Bad_fd -> badfd fd
        | `Closed -> return `Eof
        | `Ready -> go fd buffer)
      | `Error (Unix.Unix_error (EBADF, _, _)) -> badfd fd
      | `Error exn ->
        Deferred.don't_wait_for (Fd.close fd);
        raise exn
    and go fd buffer =
      if Fd.supports_nonblock fd
      then
        finish
          fd
          buffer
          (Fd.syscall fd ~nonblocking:true (fun file_descr ->
               Buffer.put buffer ~f:(fun bigstring ~off ~len ->
                   Unix.Syscall_result.Int.ok_or_unix_error_exn
                     ~syscall_name:"read"
                     (Bigstring.read_assume_fd_is_nonblocking
                        file_descr
                        bigstring
                        ~pos:off
                        ~len))))
      else
        Fd.syscall_in_thread fd ~name:"read" (fun file_descr ->
            Buffer.put buffer ~f:(fun bigstring ~off ~len ->
                Bigstring.read file_descr bigstring ~pos:off ~len))
        >>= fun result -> finish fd buffer result
    in
    go fd buffer
  ;;

  open Httpaf

  module Server = struct
    let create_connection_handler
        ?(config = Config.default)
        ~request_handler
        ~error_handler
        client_addr
        socket
      =
      let fd = Socket.fd socket in
      let writev = Faraday_async.writev_of_fd fd in
      let request_handler = request_handler client_addr in
      let error_handler = error_handler client_addr in
      let conn = Server_connection.create ~config ~error_handler request_handler in
      let read_complete = Ivar.create () in
      let buffer = Buffer.create config.read_buffer_size in
      let rec reader_thread () =
        match Server_connection.next_read_operation conn with
        | `Read ->
          (* Log.Global.printf "read(%d)%!" (Fd.to_int_exn fd); *)
          read fd buffer
          >>> (function
          | `Eof ->
            (Buffer.get buffer ~f:(fun bigstring ~off ~len ->
                 Server_connection.read_eof conn bigstring ~off ~len)
              : int)
            |> ignore;
            reader_thread ()
          | `Ok _ ->
            (Buffer.get buffer ~f:(fun bigstring ~off ~len ->
                 Server_connection.read conn bigstring ~off ~len)
              : int)
            |> ignore;
            reader_thread ())
        | `Yield ->
          (* Log.Global.printf "read_yield(%d)%!" (Fd.to_int_exn fd); *)
          Server_connection.yield_reader conn reader_thread
        | `Close ->
          (* Log.Global.printf "read_close(%d)%!" (Fd.to_int_exn fd); *)
          Ivar.fill read_complete ();
          if not (Fd.is_closed fd) then Socket.shutdown socket `Receive
      in
      let write_complete = Ivar.create () in
      let rec writer_thread () =
        match Server_connection.next_write_operation conn with
        | `Write iovecs ->
          (* Log.Global.printf "write(%d)%!" (Fd.to_int_exn fd); *)
          writev iovecs
          >>> fun result ->
          Server_connection.report_write_result conn result;
          writer_thread ()
        | `Yield ->
          (* Log.Global.printf "write_yield(%d)%!" (Fd.to_int_exn fd); *)
          Server_connection.yield_writer conn writer_thread
        | `Close _ ->
          (* Log.Global.printf "write_close(%d)%!" (Fd.to_int_exn fd); *)
          Ivar.fill write_complete ();
          if not (Fd.is_closed fd) then Socket.shutdown socket `Send
      in
      let conn_monitor = Monitor.create () in
      Scheduler.within ~monitor:conn_monitor reader_thread;
      Scheduler.within ~monitor:conn_monitor writer_thread;
      Monitor.detach_and_iter_errors conn_monitor ~f:(fun exn ->
          Server_connection.report_exn conn exn);
      (* The Tcp module will close the file descriptor once this becomes determined. *)
      Deferred.all_unit [ Ivar.read read_complete; Ivar.read write_complete ]
    ;;
  end
end

module Http = struct
  module Server = struct
    let listen
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
        Server.create_sock
          ?max_connections
          ?max_accepts_per_batch
          ?backlog
          ?socket
          ~on_handler_error
          where_to_listen)
        (Httpaf_async.Server.create_connection_handler ~request_handler ~error_handler)
    ;;
  end
end
