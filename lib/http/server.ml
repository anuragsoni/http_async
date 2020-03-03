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
    let body_read = Ivar.create () in
    let body = Body.read_httpaf_body ?length body_read req_body in
    let write_fixed_response ~headers f status body =
      let headers = Httpaf_http.headers_to_httpaf_headers headers in
      f reqd (Httpaf.Response.create ~headers status) body;
      Deferred.unit
    in
    match Httpaf_http.httpaf_request_to_request ~body req with
    | Error e -> Error.raise e
    | Ok request ->
      don't_wait_for
        (Ivar.read body_read
        >>= fun () ->
        service request
        >>= function
        | Error err -> Error.raise err
        | Ok { Response.body; headers; status; _ } ->
          (match body.content with
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
            Pipe.iter_without_pushback
              s
              ~continue_on_error:true
              ~f:(fun { Core.Unix.IOVec.buf; pos; len } ->
                Httpaf.Body.write_bigstring rb ~off:pos ~len buf)
            >>| fun () -> Httpaf.Body.flush rb (fun () -> Httpaf.Body.close_writer rb)))
  in
  Protocol.Server.create_connection_handler ~request_handler
;;
