open! Core
open! Async
open! Shuttle
open Ppx_log_async.No_global
module Logger = Log.Make_global ()

let log = Lazy.force Logger.log

type request = Http.Request.t * Body.Reader.t
type response = Http.Response.t * Body.Writer.t
type handler = request -> response Deferred.t

let write_response writer encoding res =
  let module Writer = Output_channel in
  let open Http in
  Writer.write writer (Version.to_string (Response.version res));
  Writer.write_char writer ' ';
  Writer.write writer (Status.to_string (Response.status res));
  Writer.write_char writer ' ';
  Writer.write writer "\r\n";
  let headers = Http.Header.add_transfer_encoding (Response.headers res) encoding in
  Header.iter
    (fun key data ->
      Writer.write writer key;
      Writer.write writer ": ";
      Writer.write writer data;
      Writer.write writer "\r\n")
    headers;
  Writer.write writer "\r\n"
;;

let run_server_loop handle_request reader writer =
  let rec loop reader writer handle_request =
    let view = Input_channel.view reader in
    match Parser.parse_request view.buf ~pos:view.pos ~len:view.len with
    | Error Partial ->
      (match%bind Input_channel.refill reader with
      | `Ok -> loop reader writer handle_request
      | `Eof | `Buffer_is_full -> Deferred.unit)
    | Error (Msg msg) ->
      [%log.error log "Error while parsing http request: %S" msg];
      Deferred.unit
    | Ok (req, consumed) ->
      Input_channel.consume reader consumed;
      let req_body = Body.Reader.Private.create req reader in
      let%bind res, res_body = handle_request (req, req_body) in
      let keep_alive =
        Http.Request.is_keep_alive req && Http.Response.is_keep_alive res
      in
      write_response writer (Body.Writer.encoding res_body) res;
      let%bind () = Body.Writer.Private.write res_body writer in
      let%bind () = Body.Reader.drain req_body in
      if keep_alive then loop reader writer handle_request else Deferred.unit
  in
  loop reader writer handle_request
;;
