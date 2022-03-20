open Core
open Async_kernel

type request = Request.t * Body.Reader.t [@@deriving sexp_of]
type response = Response.t * Body.Writer.t [@@deriving sexp_of]
type t = request -> response Deferred.t

let body request = Body.Reader.pipe (snd request)
let header request key = Headers.find (Request.headers (fst request)) key
let resource request = Request.path (fst request)
let meth request = Request.meth (fst request)

let respond_string ?(headers = []) ?(status = `Ok) body =
  let body = Body.Writer.string body in
  let headers = Headers.of_rev_list headers in
  let response = Response.create ~headers status in
  return (response, body)
;;

let respond_bigstring ?(headers = []) ?(status = `Ok) body =
  let body = Body.Writer.bigstring body in
  let headers = Headers.of_rev_list headers in
  let response = Response.create ~headers status in
  return (response, body)
;;

let respond_stream ?(headers = []) ?(status = `Ok) body =
  let body = Body.Writer.stream body in
  let headers = Headers.of_rev_list headers in
  let response = Response.create ~headers status in
  return (response, body)
;;
