open Base

let httpaf_headers_to_headers headers =
  Httpaf.Headers.to_list headers
  |> List.map ~f:(fun (key, data) ->
         let open Or_error.Let_syntax in
         let%map key = Headers.Header_key.of_string key
         and data = Headers.Header_value.of_string data in
         key, data)
  |> Or_error.combine_errors
  |> Or_error.map ~f:(fun hs ->
         List.fold hs ~init:Headers.empty ~f:(fun acc (key, data) ->
             Headers.add key data acc))
;;

let%test_unit "convert httpaf headers to Headers" =
  let h = [ "foo", "bar"; "baz", "abd"; "foo", "121" ] in
  let httpaf_headers = Httpaf.Headers.of_list h in
  let headers = httpaf_headers_to_headers httpaf_headers in
  let expect =
    Ok
      (List.fold h ~init:Headers.empty ~f:(fun acc (k, v) ->
           Headers.add
             (Headers.Header_key.of_string_exn k)
             (Headers.Header_value.of_string_exn v)
             acc))
  in
  [%test_result: Headers.t Or_error.t] headers ~expect;
  let httpaf_headers = Httpaf.Headers.of_list (("foo\129", "bba") :: h) in
  let headers = httpaf_headers_to_headers httpaf_headers in
  [%test_result: bool] (headers |> Or_error.is_error) ~expect:true
;;

let headers_to_httpaf_headers headers =
  Map.fold headers ~init:Httpaf.Headers.empty ~f:(fun ~key ~data acc ->
      let key = Headers.Header_key.to_string key in
      List.fold data ~init:acc ~f:(fun acc v ->
          Httpaf.Headers.add acc key (Headers.Header_value.to_string v)))
;;

let%test_unit "convert from Headers to Httpaf Headers and back" =
  let headers =
    [ "foo", "bar"; "AB", "pqr"; "hello", "world"; "foo", "baz"; "Hello", "world" ]
  in
  let m =
    List.fold
      headers
      ~init:(Map.empty (module String))
      ~f:(fun acc (k, v) ->
        let k = String.lowercase k in
        Map.add_multi acc ~key:k ~data:v)
  in
  let h = httpaf_headers_to_headers (Httpaf.Headers.of_list headers) |> Or_error.ok_exn in
  let h' = headers_to_httpaf_headers h in
  let m' = Map.of_alist_multi (module String) (Httpaf.Headers.to_list h') in
  [%test_result: string list Map.M(String).t] ~expect:m m'
;;

let httpaf_response_to_response resp body =
  let headers = resp.Httpaf.Response.headers in
  let status = resp.Httpaf.Response.status in
  match httpaf_headers_to_headers headers with
  | Error _ as e -> e
  | Ok headers -> Or_error.return (Response.make ~headers ~body status)
;;
