open Core
open Async

type status = Httpaf.Status.t

let sexp_of_status s = sexp_of_string (Format.asprintf "%a" Httpaf.Status.pp_hum s)

type t =
  { status : status
  ; headers : Headers.t
  ; body : Body.t
  }
[@@deriving sexp_of, fields]

let create ?(headers = Headers.empty) ?(body = Body.empty) status =
  { status; headers; body }
;;

let pp fmt t = Sexp.pp fmt (sexp_of_t t)
let pp_hum fmt t = Sexp.pp_hum fmt (sexp_of_t t)

let of_string ?(headers = Headers.empty) ?(status = `OK) body =
  let headers =
    Headers.add_unless_exists
      ~key:"content-length"
      ~value:(Int.to_string (String.length body))
      headers
  in
  return (create ~headers ~body:(Body.of_string body) status)
;;

let of_bigstring ?(headers = Headers.empty) ?(status = `OK) body =
  let headers =
    Headers.add_unless_exists
      ~key:"content-length"
      ~value:(Int.to_string (Bigstring.length body))
      headers
  in
  return (create ~headers ~body:(Body.of_bigstring body) status)
;;

let of_stream ?(headers = Headers.empty) ?(status = `OK) ?size f =
  let reader = Pipe.create_reader ~close_on_exception:false f in
  let headers =
    match size with
    | None -> Headers.add_unless_exists ~key:"transfer-encoding" ~value:"chunked" headers
    | Some s ->
      Headers.add_unless_exists ~key:"content-length" ~value:(Int64.to_string s) headers
  in
  return (create ~headers ~body:(Body.of_stream reader) status)
;;

let of_file ?(headers = Headers.empty) ?(status = `OK) name =
  let module Unix = Core.Unix in
  Monitor.try_with ~run:`Now ~here:[%here] ~name:"static file handler" (fun () ->
      let%bind stat = Async_unix.Unix.stat name in
      match Async_unix.Unix.Stats.kind stat with
      | `File ->
        let%map reader = Reader.open_file name in
        let size = Async_unix.Unix.Stats.size stat in
        let mime = Magic_mime.lookup name in
        let headers = Headers.add_unless_exists ~key:"content-type" ~value:mime headers in
        let write_pipe writer =
          match%map
            Reader.read_one_chunk_at_a_time reader ~handle_chunk:(fun chunk ~pos ~len ->
                let%map () =
                  Pipe.write_if_open writer (Unix.IOVec.of_bigstring chunk ~pos ~len)
                in
                `Consumed (len, `Need_unknown))
          with
          | `Stopped () -> assert false
          | `Eof_with_unconsumed_data d ->
            let d' = Bigstring.of_string d in
            Pipe.write_without_pushback_if_open writer (Unix.IOVec.of_bigstring d')
          | `Eof -> ()
        in
        of_stream ~headers ~status ~size write_pipe
      | _ -> Error.raise (Error.of_thunk (fun () -> "Not a file")))
  >>= function
  | Ok r -> r
  | Error _ -> (* TODO: Log this somehow *) of_string ~status:`Not_found "File not found"
;;
