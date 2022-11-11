open Core
open Async
open Shuttle

module Reader = struct
  type t =
    | Empty
    | Stream of
        { encoding : [ `Chunked | `Fixed of int ]
        ; reader : (Bigstring.t Core_unix.IOVec.t Pipe.Reader.t[@sexp.opaque])
        }
  [@@deriving sexp_of]

  let empty = Empty

  module Private = struct
    let rec read_bigstring chan len =
      let view = Input_channel.view chan in
      if view.len > 0
      then (
        let to_read = min len view.len in
        return (`Ok (Core_unix.IOVec.of_bigstring ~pos:view.pos ~len:to_read view.buf)))
      else
        Input_channel.refill chan
        >>= function
        | `Eof -> return `Eof
        | `Ok -> read_bigstring chan len
    ;;

    let fixed_reader len chan =
      Pipe.create_reader ~close_on_exception:false (fun writer ->
        Deferred.repeat_until_finished len (fun len ->
          read_bigstring chan len
          >>= function
          | `Eof -> return (`Finished ())
          | `Ok chunk ->
            let consumed = chunk.len in
            Pipe.write_if_open writer chunk
            >>= fun () ->
            Pipe.downstream_flushed writer
            >>= fun _ ->
            Input_channel.consume chan consumed;
            if consumed = len
            then return (`Finished ())
            else return (`Repeat (len - consumed))))
    ;;

    let chunked_reader chan =
      Pipe.create_reader ~close_on_exception:false (fun writer ->
        Deferred.repeat_until_finished Parser.Start_chunk (fun state ->
          let view = Input_channel.view chan in
          match Parser.parse_chunk ~pos:view.pos ~len:view.len view.buf state with
          | Error (Fail error) -> Error.raise error
          | Error Partial ->
            Input_channel.refill chan
            >>| (function
            | `Ok -> `Repeat state
            | `Eof -> `Finished ())
          | Ok (parse_result, consumed) ->
            (match parse_result with
             | Parser.Chunk_complete chunk ->
               Pipe.write_if_open writer chunk
               >>= fun () ->
               Pipe.downstream_flushed writer
               >>| fun _ ->
               Input_channel.consume chan consumed;
               `Repeat Parser.Start_chunk
             | Parser.Done -> return (`Finished ())
             | Parser.Partial_chunk (chunk, to_consume) ->
               Pipe.write_if_open writer chunk
               >>= fun () ->
               Pipe.downstream_flushed writer
               >>| fun _ ->
               Input_channel.consume chan consumed;
               `Repeat (Parser.Continue_chunk to_consume))))
    ;;

    let get_transfer_encoding headers =
      match List.rev @@ Headers.find_multi headers "Transfer-Encoding" with
      | x :: _ when String.Caseless.equal x "chunked" -> `Chunked
      | _x :: _ -> `Bad_request
      | [] ->
        (match
           List.dedup_and_sort
             ~compare:String.Caseless.compare
             (Headers.find_multi headers "Content-Length")
         with
         | [] -> `Fixed 0
         (* TODO: check for exceptions when converting to int *)
         | [ x ] ->
           let len =
             try Int.of_string x with
             | _ -> -1
           in
           if Int.(len >= 0) then `Fixed len else `Bad_request
         | _ -> `Bad_request)
    ;;

    let create req chan =
      match get_transfer_encoding (Request.headers req) with
      | `Fixed 0 -> Ok empty
      | `Fixed len as encoding ->
        let reader = fixed_reader len chan in
        Ok (Stream { encoding; reader })
      | `Chunked as encoding -> Ok (Stream { encoding; reader = chunked_reader chan })
      | `Bad_request -> Or_error.error_s [%sexp "Invalid transfer encoding"]
    ;;
  end

  let encoding t =
    match t with
    | Empty -> `Fixed 0
    | Stream { encoding; _ } -> encoding
  ;;

  let pipe t =
    match t with
    | Empty -> Pipe.empty ()
    | Stream { reader; _ } -> reader
  ;;
end

module Writer = struct
  type kind =
    | Empty
    | String of string
    | Bigstring of Bigstring.t
    | Stream of (Bigstring.t Core_unix.IOVec.t Pipe.Reader.t[@sexp.opaque])
  [@@deriving sexp_of]

  type t =
    { encoding : [ `Chunked | `Fixed of int ]
    ; kind : kind
    }
  [@@deriving sexp_of]

  let encoding t = t.encoding
  let empty = { encoding = `Fixed 0; kind = Empty }
  let string x = { encoding = `Fixed (Int.of_int (String.length x)); kind = String x }

  let bigstring x =
    { encoding = `Fixed (Int.of_int (Bigstring.length x)); kind = Bigstring x }
  ;;

  let stream ?(encoding = `Chunked) x = { encoding; kind = Stream x }

  module Private = struct
    let is_chunked t =
      match t.encoding with
      | `Chunked -> true
      | _ -> false
    ;;

    let make_writer t =
      match t.encoding with
      | `Chunked ->
        fun writer buf ->
          (* avoid writing empty payloads as that is used to indicate the end of a
             stream. *)
          if buf.Core_unix.IOVec.len = 0
          then Deferred.unit
          else (
            Output_channel.writef writer "%x\r\n" buf.len;
            Output_channel.write_bigstring writer buf.buf ~pos:buf.pos ~len:buf.len;
            Output_channel.write writer "\r\n";
            Output_channel.flush writer)
      | `Fixed _ ->
        fun writer buf ->
          if buf.len = 0
          then Deferred.unit
          else (
            Output_channel.write_bigstring writer buf.buf ~pos:buf.pos ~len:buf.len;
            Output_channel.flush writer)
    ;;

    let write t writer =
      Deferred.create (fun ivar ->
        match t.kind with
        | Empty -> Output_channel.flush writer >>> fun () -> Ivar.fill ivar ()
        | String x ->
          Output_channel.write writer x;
          Output_channel.flush writer >>> fun () -> Ivar.fill ivar ()
        | Bigstring b ->
          Output_channel.write_bigstring writer b;
          Output_channel.flush writer >>> fun () -> Ivar.fill ivar ()
        | Stream xs ->
          let write_chunk = make_writer t in
          Pipe.iter ~flushed:Pipe.Flushed.When_value_processed xs ~f:(fun buf ->
            write_chunk writer buf)
          >>> fun () ->
          if is_chunked t
          then (
            Output_channel.write writer "0\r\n\r\n";
            Output_channel.flush writer >>> fun () -> Ivar.fill ivar ())
          else Ivar.fill ivar ())
    ;;
  end
end
