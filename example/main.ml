open Core
open Async

let text =
  "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor \
   incididunt ut labore et dolore magna aliqua. Pretium vulputate sapien nec sagittis. \
   Ante in nibh mauris cursus mattis molestie a iaculis. Diam vulputate ut pharetra sit \
   amet. Morbi tristique senectus et netus et malesuada fames ac. Sed sed risus pretium \
   quam vulputate dignissim suspendisse. Magna sit amet purus gravida quis blandit. Id \
   aliquet lectus proin nibh. Aliquet porttitor lacus luctus accumsan tortor posuere. \
   Leo in vitae turpis massa sed elementum tempus egestas. Nec ullamcorper sit amet \
   risus nullam eget felis eget. Quam viverra orci sagittis eu volutpat odio facilisis. \
   Auctor urna nunc id cursus metus aliquam. Phasellus vestibulum lorem sed risus \
   ultricies tristique nulla aliquet enim. Odio eu feugiat pretium nibh ipsum. Sed \
   vulputate mi sit amet mauris commodo quis imperdiet massa. Nunc aliquet bibendum enim \
   facilisis gravida neque convallis a. Placerat in egestas erat imperdiet sed euismod \
   nisi porta. Magna eget est lorem ipsum dolor sit amet consectetur. Pretium aenean \
   pharetra magna ac placerat.\n\n\
   Senectus et netus et malesuada fames. Cursus metus aliquam eleifend mi. Purus in \
   mollis nunc sed. Condimentum id venenatis a condimentum vitae. Morbi enim nunc \
   faucibus a. Rhoncus est pellentesque elit ullamcorper dignissim cras. Sed risus \
   ultricies tristique nulla aliquet enim tortor at auctor. Pulvinar neque laoreet \
   suspendisse interdum consectetur libero id faucibus. Phasellus egestas tellus rutrum \
   tellus pellentesque eu tincidunt tortor. Dictum at tempor commodo ullamcorper a.\n\n\
   Urna neque viverra justo nec ultrices dui sapien eget mi. Consectetur a erat nam at \
   lectus urna. Ac turpis egestas sed tempus urna et. Nibh tortor id aliquet lectus \
   proin nibh. Sit amet porttitor eget dolor. Augue ut lectus arcu bibendum. Montes \
   nascetur ridiculus mus mauris. Habitasse platea dictumst vestibulum rhoncus est \
   pellentesque elit. Cras ornare arcu dui vivamus arcu. Id porta nibh venenatis cras \
   sed. Fermentum odio eu feugiat pretium nibh ipsum consequat nisl vel. Odio tempor \
   orci dapibus ultrices in iaculis nunc sed. Mattis vulputate enim nulla aliquet \
   porttitor.\n\n\
   Lacus suspendisse faucibus interdum posuere lorem ipsum dolor sit. Tellus \
   pellentesque eu tincidunt tortor aliquam nulla facilisi. Ultrices gravida dictum \
   fusce ut placerat orci. At in tellus integer feugiat scelerisque varius morbi. Nulla \
   pharetra diam sit amet nisl suscipit adipiscing. Metus vulputate eu scelerisque felis \
   imperdiet proin fermentum. Diam maecenas sed enim ut. Eget dolor morbi non arcu risus \
   quis. Suspendisse sed nisi lacus sed viverra tellus in hac habitasse. Quam viverra \
   orci sagittis eu volutpat. Consequat ac felis donec et. Vitae purus faucibus ornare \
   suspendisse sed nisi lacus sed viverra. Donec massa sapien faucibus et molestie. Duis \
   ut diam quam nulla porttitor massa id neque aliquam. Venenatis lectus magna fringilla \
   urna porttitor rhoncus. Dui sapien eget mi proin sed libero enim. Purus viverra \
   accumsan in nisl nisi. At augue eget arcu dictum varius duis at consectetur. Semper \
   viverra nam libero justo laoreet. Dis parturient montes nascetur ridiculus mus mauris \
   vitae ultricies.\n\n\
   Lorem mollis aliquam ut porttitor leo a diam sollicitudin. Vel quam elementum \
   pulvinar etiam non quam lacus suspendisse. Tincidunt praesent semper feugiat nibh sed \
   pulvinar proin gravida. Vitae auctor eu augue ut. Lacus sed viverra tellus in hac \
   habitasse platea. Eros donec ac odio tempor. Eu facilisis sed odio morbi. Purus sit \
   amet luctus venenatis lectus magna fringilla urna porttitor. Leo vel orci porta non \
   pulvinar neque. Sollicitudin nibh sit amet commodo nulla. Ut venenatis tellus in \
   metus vulputate eu scelerisque felis.\n\n\
   Purus non enim praesent elementum facilisis leo. Vestibulum sed arcu non odio \
   euismod. Donec pretium vulputate sapien nec sagittis aliquam malesuada bibendum. \
   Volutpat odio facilisis mauris sit amet massa vitae tortor. Eget mi proin sed libero \
   enim. Faucibus in ornare quam viverra orci sagittis eu. Tincidunt nunc pulvinar \
   sapien et ligula ullamcorper. Vel pretium lectus quam id leo in vitae turpis massa. \
   Tellus integer feugiat scelerisque varius morbi. Elementum integer enim neque \
   volutpat ac tincidunt vitae semper quis. Non arcu risus quis varius. Amet consectetur \
   adipiscing elit duis. Diam sollicitudin tempor id eu."
;;

let text = Bigstring.of_string text

let request_handler _ =
  let open Async_http in
  return (Response.of_bigstring text)
;;

let error_handler _ ?request:_ error start_response =
  let open Httpaf in
  let response_body = start_response Headers.empty in
  (match error with
  | `Exn exn ->
    Body.write_string response_body (Exn.to_string exn);
    Body.write_string response_body "\n"
  | #Status.standard as error ->
    Body.write_string response_body (Status.default_reason_phrase error));
  Body.close_writer response_body
;;

let main port () =
  let where_to_listen =
    Tcp.Where_to_listen.bind_to
      Tcp.Bind_to_address.Localhost
      (Tcp.Bind_to_port.On_port port)
  in
  Async_http.Server.listen
    ~on_handler_error:`Ignore
    ~request_handler
      (* ~crt_file:"./certs/localhost.pem" *)
      (* ~key_file:"./certs/localhost.key" *)
    ~error_handler
    where_to_listen
  >>= fun server ->
  Log.Global.info "Listening on http://localhost:%d" port;
  Deferred.forever () (fun () ->
      Clock.after Time.Span.(of_sec 0.5)
      >>| fun () -> Log.Global.info "conns: %d" (Tcp.Server.num_connections server));
  Deferred.never ()
;;

let () =
  Log.Global.set_level (Log.Level.of_string "Info");
  Command.async
    ~summary:"Sample server"
    Command.Param.(
      map
        (flag "-p" (optional_with_default 8080 int) ~doc:"int Source port to listen on")
        ~f:(fun port () -> main port ()))
  |> Command.run
;;
