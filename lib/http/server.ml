open Base
open Async_kernel

let create_connection_handler service =
  let request_handler _conn reqd =
    let req = Httpaf.Reqd.request reqd in
    let req_body = Httpaf.Reqd.request_body reqd in
    let length =
      match Httpaf.Request.body_length req with
      | `Chunked -> None
      | `Fixed l -> Some l
      | `Error _ -> Error.raise_s [%message "Bad request"]
    in
    let body =
      Body.read_httpaf_body ?length (fun () -> Httpaf.Body.close_reader req_body) req_body
    in
    let write_fixed_response ~headers f status body =
      let headers = Httpaf_http.headers_to_httpaf_headers headers in
      f reqd (Httpaf.Response.create ~headers status) body;
      Deferred.unit
    in
    let request = Httpaf_http.httpaf_request_to_request ~body req in
    don't_wait_for
      (let%bind { Response.body; headers; status; _ } = service request in
       let { Body.content; length } = body in
       let headers =
         match length with
         | None -> Headers.add_if_missing "transfer-encoding" "chunked" headers
         | Some l -> Headers.add_if_missing "content-length" (Int64.to_string l) headers
       in
       match content with
       | Body.Empty ->
         write_fixed_response ~headers Httpaf.Reqd.respond_with_string status ""
       | String s ->
         write_fixed_response ~headers Httpaf.Reqd.respond_with_string status s
       | Bigstring b ->
         write_fixed_response ~headers Httpaf.Reqd.respond_with_bigstring status b
       | Stream s ->
         let rb =
           Httpaf.Reqd.respond_with_streaming
             reqd
             (Httpaf.Response.create
                ~headers:(Httpaf_http.headers_to_httpaf_headers headers)
                status)
         in
         let%map () =
           Pipe.iter_without_pushback s ~continue_on_error:true ~f:(fun w ->
               Httpaf.Body.write_string rb w)
         in
         Httpaf.Body.flush rb (fun () -> Httpaf.Body.close_writer rb))
  in
  Protocol.Server.create_connection_handler ~request_handler
;;
