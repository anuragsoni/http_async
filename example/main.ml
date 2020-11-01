open Core
open Async

let stdout = Lazy.force Writer.stdout

let text =
  "CHAPTER I. Down the Rabbit-Hole  Alice was beginning to get very tired of sitting by \
   her sister on the bank, and of having nothing to do: once or twice she had peeped \
   into the book her sister was reading, but it had no pictures or conversations in it, \
   <and what is the use of a book,> thought Alice <without pictures or conversations?> \
   So she was considering in her own mind (as well as she could, for the hot day made \
   her feel very sleepy and stupid), whether the pleasure of making a daisy-chain would \
   be worth the trouble of getting up and picking the daisies, when suddenly a White \
   Rabbit with pink eyes ran close by her. There was nothing so very remarkable in that; \
   nor did Alice think it so very much out of the way to hear the Rabbit say to itself, \
   <Oh dear! Oh dear! I shall be late!> (when she thought it over afterwards, it \
   occurred to her that she ought to have wondered at this, but at the time it all \
   seemed quite natural); but when the Rabbit actually took a watch out of its \
   waistcoat-pocket, and looked at it, and then hurried on, Alice started to her feet, \
   for it flashed across her mind that she had never before seen a rabbit with either a \
   waistcoat-pocket, or a watch to take out of it, and burning with curiosity, she ran \
   across the field after it, and fortunately was just in time to see it pop down a \
   large rabbit-hole under the hedge. In another moment down went Alice after it, never \
   once considering how in the world she was to get out again. The rabbit-hole went \
   straight on like a tunnel for some way, and then dipped suddenly down, so suddenly \
   that Alice had not a moment to think about stopping herself before she found herself \
   falling down a very deep well. Either the well was very deep, or she fell very \
   slowly, for she had plenty of time as she went down to look about her and to wonder \
   what was going to happen next. First, she tried to look down and make out what she \
   was coming to, but it was too dark to see anything; then she looked at the sides of \
   the well, and noticed that they were filled with cupboards......"
;;

let text = Bigstringaf.of_string ~off:0 ~len:(String.length text) text

let request_handler reqd =
  let open Httpaf in
  let req_body = Reqd.request_body reqd in
  let on_eof () =
    don't_wait_for (Writer.flushed stdout);
    let headers =
      Httpaf.Headers.of_list [ "Content-Length", Bigstring.length text |> Int.to_string ]
    in
    Reqd.respond_with_bigstring reqd (Response.create ~headers `OK) text
  in
  let rec on_read buf ~off ~len =
    Writer.write_bigstring stdout buf ~pos:off ~len;
    Body.schedule_read req_body ~on_eof ~on_read
  in
  Body.schedule_read req_body ~on_eof ~on_read
;;

let run port =
  let where_to_listen = Tcp.Where_to_listen.of_port port in
  let error_handler = `Raise in
  let handler = Async_http_protocol.Server.create_connection_handler ~request_handler in
  let server =
    Async_connection.Server.create ~backlog:11_000 error_handler where_to_listen handler
  in
  Log.Global.printf
    ~level:`Info
    "Server listening on: http://localhost:%d"
    (Tcp.Server.listening_on server);
  Deferred.never ()
;;

let command =
  Command.async
    ~summary:"async http"
    Command.Param.(
      let open Command.Let_syntax in
      let%map port = flag "-port" (optional_with_default 8080 int) ~doc:"int PORT" in
      fun () -> run port)
;;

let () = Command.run command
