open Core_kernel

let httpaf_headers_to_headers headers =
  Httpaf.Headers.to_list headers
  |> List.fold ~init:Headers.empty ~f:(fun acc (key, data) -> Headers.add key data acc)
;;

let%test_unit "convert httpaf headers to Headers" =
  let h = [ "foo", "bar"; "baz", "abd"; "foo", "121" ] in
  let httpaf_headers = Httpaf.Headers.of_list h in
  let headers = httpaf_headers_to_headers httpaf_headers in
  let expect =
    List.fold h ~init:Headers.empty ~f:(fun acc (k, v) -> Headers.add k v acc)
  in
  [%test_result: Headers.t] headers ~expect
;;

let headers_to_httpaf_headers headers =
  Map.fold headers ~init:Httpaf.Headers.empty ~f:(fun ~key ~data acc ->
      List.fold data ~init:acc ~f:(fun acc v -> Httpaf.Headers.add acc key v))
;;

let%test_unit "convert from Headers to Httpaf Headers and back" =
  let headers =
    [ "foo", "bar"; "AB", "pqr"; "hello", "world"; "foo", "baz"; "Hello", "world" ]
  in
  let m =
    List.fold headers ~init:String.Caseless.Map.empty ~f:(fun acc (k, v) ->
        Map.add_multi acc ~key:k ~data:v)
  in
  let h = httpaf_headers_to_headers (Httpaf.Headers.of_list headers) in
  let h' = headers_to_httpaf_headers h in
  let m' = String.Caseless.Map.of_alist_multi (Httpaf.Headers.to_list h') in
  [%test_result: string list String.Caseless.Map.t] ~expect:m m'
;;

let httpaf_response_to_response resp body =
  let headers = resp.Httpaf.Response.headers in
  let status = resp.Httpaf.Response.status in
  let headers = httpaf_headers_to_headers headers in
  Response.make ~headers ~body status
;;

let httpaf_request_to_request ?body req =
  let headers = req.Httpaf.Request.headers in
  let meth = req.meth in
  let headers = httpaf_headers_to_headers headers in
  Request.make ~headers ?body meth req.target
;;
