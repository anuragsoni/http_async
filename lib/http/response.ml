open Core
open Async
module Unix = Core.Unix

type status = Httpaf.Status.t

let sexp_of_status t = sexp_of_int (Httpaf.Status.to_code t)

type t =
  { status : status
  ; headers : Headers.t
  ; body : Body.t
  ; state : Univ_map.t
  }
[@@deriving sexp_of]

let make ?(headers = Headers.empty) ?(body = Body.empty) status =
  { status; headers; body; state = Univ_map.empty }
;;

let of_string ?headers ?(status = `OK) b =
  return (make ?headers ~body:(Body.of_string b) status)
;;

let of_bigstring ?headers ?(status = `OK) b =
  return (make ?headers ~body:(Body.of_bigstring b) status)
;;

let of_file ?(headers = Headers.empty) ?(status = `OK) name =
  Monitor.try_with ~run:`Now ~here:[%here] ~name:"static file response" (fun () ->
      let%bind stat = Async_unix.Unix.stat name in
      match Async_unix.Unix.Stats.kind stat with
      | `File ->
        let%map reader = Reader.open_file name in
        let size = Async_unix.Unix.Stats.size stat in
        let mime = Magic_mime.lookup name in
        let headers = Headers.add_if_missing "content-type" mime headers in
        let reader_pipe = Reader.pipe reader in
        let body = Body.of_pipe ~length:size reader_pipe in
        return (make ~headers ~body status)
      | _ -> Error.raise (Error.of_thunk (fun () -> "Not a file")))
  >>= function
  | Ok r -> r
  | Error _ -> return (make ~body:(Body.of_string "File not found") `Not_found)
;;
