open Base
open Async_kernel

let create_error_handler handler =
  let error_handler ?request error start_response =
    let req_headers =
      Option.map request ~f:(fun req -> req.Httpaf.Request.headers)
      |> Option.value ~default:Httpaf.Headers.empty
      |> Httpaf_http.httpaf_headers_to_headers
    in
    don't_wait_for
      (let%bind headers, body = handler req_headers error in
       let length = Body.length body in
       let headers =
         match length with
         | None -> headers
         | Some l -> Headers.add_if_missing "content-length" (Int64.to_string l) headers
       in
       let res_body = start_response (Httpaf_http.headers_to_httpaf_headers headers) in
       let%map () =
         Pipe.iter_without_pushback (Body.to_pipe body) ~f:(fun v ->
             Httpaf.Body.write_string res_body v)
       in
       Httpaf.Body.close_writer res_body)
  in
  error_handler
;;

type error_handler =
  Headers.t -> Httpaf.Server_connection.error -> (Headers.t * Body.t) Deferred.t

let create_connection_handler ?error_handler service =
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
      let reader = Httpaf_http.read_httpaf_body req_body in
      upon (Pipe.closed reader) (fun () -> Httpaf.Body.close_reader req_body);
      Body.of_pipe ?length reader
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
       let%map () =
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
           Httpaf.Body.flush rb (fun () -> Httpaf.Body.close_writer rb)
       in
       (* Its possible that the user did not consume the pipe inside the handler.
          We can perform cleanup here so the request_body can be closed. *)
       Pipe.close_read (Body.to_pipe request.body))
  in
  let error_handler = Option.map error_handler ~f:create_error_handler in
  Protocol.Server.create_connection_handler ?error_handler ~request_handler
;;
