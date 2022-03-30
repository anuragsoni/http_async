open Core

exception Fail of Error.t
exception Partial

let[@inline always] is_tchar = function
  | '0' .. '9'
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '!'
  | '#'
  | '$'
  | '%'
  | '&'
  | '\''
  | '*'
  | '+'
  | '-'
  | '.'
  | '^'
  | '_'
  | '`'
  | '|'
  | '~' -> true
  | _ -> false
;;

module Source = struct
  type t =
    { buffer : Bigstring.t
    ; mutable pos : int
    ; upper_bound : int
    }

  let of_bytes ~pos ?len buffer =
    let buf_len = Bigstring.length buffer in
    if pos < 0 || pos > buf_len
    then
      invalid_arg
        (Printf.sprintf
           "Http_parser.Source.of_bigstring: Invalid offset %d. Buffer length: %d"
           pos
           buf_len);
    let len = Option.value len ~default:(buf_len - pos) in
    if len < 0 || pos + len > buf_len
    then
      invalid_arg
        (Printf.sprintf
           "Http_parser.Source.of_bigstring: Invalid len %d. offset: %d, buffer_length: \
            %d, requested_length: %d"
           len
           pos
           buf_len
           (pos + len));
    { buffer; pos; upper_bound = pos + len }
  ;;

  let[@inline always] get_unsafe t idx = Bigstring.get t.buffer (t.pos + idx)

  let[@inline always] get t idx =
    if idx < 0 || t.pos + idx >= t.upper_bound
    then invalid_arg "Http_parser.Source.get: Index out of bounds";
    Bigstring.get t.buffer (t.pos + idx)
  ;;

  let[@inline always] advance_unsafe t count = t.pos <- t.pos + count

  let[@inline always] advance t count =
    if count < 0 || t.pos + count > t.upper_bound
    then
      invalid_arg
        (Printf.sprintf
           "Http_parser.Source.advance: Index out of bounds. Requested count: %d"
           count);
    t.pos <- t.pos + count
  ;;

  let[@inline always] length t = t.upper_bound - t.pos
  let[@inline always] is_empty t = t.pos = t.upper_bound

  let[@inline always] to_string t ~pos ~len =
    if pos < 0
       || t.pos + pos >= t.upper_bound
       || len < 0
       || t.pos + pos + len > t.upper_bound
    then
      invalid_arg
        (Format.asprintf
           "Http_parser.Source.substring: Index out of bounds., Requested off: %d, len: \
            %d"
           pos
           len);
    let b = Bytes.create len in
    Bigstring.To_bytes.unsafe_blit
      ~src:t.buffer
      ~dst:b
      ~src_pos:(t.pos + pos)
      ~dst_pos:0
      ~len;
    Bytes.unsafe_to_string ~no_mutation_while_string_reachable:b
  ;;

  let[@inline always] is_space = function
    | ' ' | '\012' | '\n' | '\r' | '\t' -> true
    | _ -> false
  ;;

  let[@inline always] to_string_trim t ~pos ~len =
    if pos < 0
       || t.pos + pos >= t.upper_bound
       || len < 0
       || t.pos + pos + len > t.upper_bound
    then
      invalid_arg
        (Format.asprintf
           "Http_parser.Source.substring: Index out of bounds., Requested off: %d, len: \
            %d"
           pos
           len);
    let last = ref (t.pos + len - 1) in
    let pos = ref (t.pos + pos) in
    while is_space (Bigstring.get t.buffer !pos) do
      incr pos
    done;
    while is_space (Bigstring.get t.buffer !last) do
      decr last
    done;
    let len = !last - !pos + 1 in
    let b = Bytes.create len in
    Bigstring.To_bytes.unsafe_blit ~src:t.buffer ~dst:b ~src_pos:!pos ~dst_pos:0 ~len;
    Bytes.unsafe_to_string ~no_mutation_while_string_reachable:b
  ;;

  let[@inline always] index t ch =
    let idx = Bigstring.unsafe_find t.buffer ch ~pos:t.pos ~len:(length t) in
    if idx < 0 then -1 else idx - t.pos
  ;;

  let unsafe_memcmp t pos str =
    let rec loop t pos str len =
      if pos = len
      then true
      else
        Char.equal (get_unsafe t pos) (String.unsafe_get str pos)
        && loop t (pos + 1) str len
    in
    loop t pos str (String.length str)
  ;;
end

let string str source =
  let len = String.length str in
  if Source.length source < len
  then raise_notrace Partial
  else if Source.unsafe_memcmp source 0 str
  then Source.advance source len
  else raise_notrace (Fail (Error.create "Could not match string" str sexp_of_string))
;;

let any_char source =
  if Source.is_empty source
  then raise_notrace Partial
  else (
    let c = Source.get_unsafe source 0 in
    Source.advance_unsafe source 1;
    c)
;;

let eol source = string "\r\n" source

let token source =
  let pos = Source.index source ' ' in
  if pos = -1
  then raise_notrace Partial
  else (
    let res = Source.to_string source ~pos:0 ~len:pos in
    Source.advance source (pos + 1);
    res)
;;

let[@inline always] ( .![] ) source idx = Source.get_unsafe source idx
let invalid_method = Fail (Error.of_string "Invalid Method")

let meth source =
  let pos = Source.index source ' ' in
  if pos = -1 then raise_notrace Partial;
  let ( = ) = Char.( = ) in
  let meth =
    match pos with
    | 3 ->
      if source.![0] = 'G' && source.![1] = 'E' && source.![2] = 'T'
      then `GET
      else if source.![0] = 'P' && source.![1] = 'U' && source.![3] = 'T'
      then `PUT
      else raise_notrace invalid_method
    | 4 ->
      if source.![0] = 'H' && source.![1] = 'E' && source.![2] = 'A' && source.![3] = 'D'
      then `HEAD
      else if source.![0] = 'P'
              && source.![1] = 'O'
              && source.![2] = 'S'
              && source.![3] = 'T'
      then `POST
      else raise_notrace invalid_method
    | 5 ->
      if source.![0] = 'P'
         && source.![1] = 'A'
         && source.![2] = 'T'
         && source.![3] = 'C'
         && source.![4] = 'H'
      then `PATCH
      else if source.![0] = 'T'
              && source.![1] = 'R'
              && source.![2] = 'A'
              && source.![3] = 'C'
              && source.![4] = 'E'
      then `TRACE
      else raise_notrace invalid_method
    | 6 ->
      if source.![0] = 'D'
         && source.![1] = 'E'
         && source.![2] = 'L'
         && source.![3] = 'E'
         && source.![4] = 'T'
         && source.![5] = 'E'
      then `DELETE
      else raise_notrace invalid_method
    | 7 ->
      if source.![0] = 'C'
         && source.![1] = 'O'
         && source.![2] = 'N'
         && source.![3] = 'N'
         && source.![4] = 'E'
         && source.![5] = 'C'
         && source.![6] = 'T'
      then `CONNECT
      else if source.![0] = 'O'
              && source.![1] = 'P'
              && source.![2] = 'T'
              && source.![3] = 'I'
              && source.![4] = 'O'
              && source.![5] = 'N'
              && source.![6] = 'S'
      then `OPTIONS
      else raise_notrace invalid_method
    | _ -> raise_notrace invalid_method
  in
  Source.advance_unsafe source (pos + 1);
  meth
;;

let version_source source =
  string "HTTP/1." source;
  any_char source
;;

let version source =
  let ch = version_source source in
  match ch with
  | '1' -> Version.Http_1_1
  | _ -> raise_notrace (Fail (Error.create "Invalid http version" ch sexp_of_char))
;;

let invalid_header_err = Fail (Error.of_string "Invalid Header Key")

let header source =
  let pos = Source.index source ':' in
  if pos = -1
  then raise_notrace Partial
  else if pos = 0
  then raise_notrace (Fail (Error.of_string "Invalid header: Empty header key"));
  for idx = 0 to pos - 1 do
    if not (is_tchar (Source.get_unsafe source idx)) then raise_notrace invalid_header_err
  done;
  let key = Source.to_string source ~pos:0 ~len:pos in
  Source.advance_unsafe source (pos + 1);
  while (not (Source.is_empty source)) && Char.(Source.get_unsafe source 0 = ' ') do
    Source.advance_unsafe source 1
  done;
  let pos = Source.index source '\r' in
  if pos = -1 then raise_notrace Partial;
  let v = Source.to_string_trim source ~pos:0 ~len:pos in
  Source.advance_unsafe source pos;
  key, v
;;

let rec headers source =
  if (not (Source.is_empty source)) && Char.(Source.get_unsafe source 0 = '\r')
  then (
    eol source;
    [])
  else (
    let header = header source in
    eol source;
    header :: headers source)
;;

let chunk_length source =
  let length = ref 0 in
  let stop = ref false in
  let state = ref `Ok in
  let count = ref 0 in
  let processing_chunk = ref true in
  let in_chunk_extension = ref false in
  while not !stop do
    if Source.is_empty source
    then (
      stop := true;
      state := `Partial)
    else if !count = 16 && not !in_chunk_extension
    then (
      stop := true;
      state := `Chunk_too_big)
    else (
      let ch = Source.get source 0 in
      Source.advance source 1;
      incr count;
      match ch with
      | '0' .. '9' as ch when !processing_chunk ->
        let curr = Char.to_int ch - Char.to_int '0' in
        length := (!length lsl 4) lor curr
      | 'a' .. 'f' as ch when !processing_chunk ->
        let curr = Char.to_int ch - Char.to_int 'a' + 10 in
        length := (!length lsl 4) lor curr
      | 'A' .. 'F' as ch when !processing_chunk ->
        let curr = Char.to_int ch - Char.to_int 'A' + 10 in
        length := (!length lsl 4) lor curr
      | ';' when not !in_chunk_extension ->
        in_chunk_extension := true;
        processing_chunk := false
      | ('\t' | ' ') when !processing_chunk -> processing_chunk := false
      | ('\t' | ' ') when (not !in_chunk_extension) && not !processing_chunk -> ()
      | '\r' ->
        if Source.is_empty source
        then (
          stop := true;
          state := `Partial)
        else if Char.(Source.get source 0 = '\n')
        then (
          Source.advance source 1;
          stop := true)
        else (
          stop := true;
          state := `Expected_newline)
      | _ when !in_chunk_extension ->
        (* Chunk extensions aren't very common, see:
           https://tools.ietf.org/html/rfc7230#section-4.1.1 Chunk extensions aren't
           pre-defined, and they are specific to invidividual connections. In the future
           we might surface these to the user somehow, but for now we will ignore any
           extensions. TODO: Should there be any limit on the size of chunk extensions we
           parse? We might want to error if a request contains really large chunk
           extensions. *)
        ()
      | ch ->
        stop := true;
        state := `Invalid_char ch)
  done;
  match !state with
  | `Ok -> !length
  | `Partial -> raise_notrace Partial
  | `Expected_newline -> raise_notrace (Fail (Error.of_string "Expected_newline"))
  | `Chunk_too_big -> raise_notrace (Fail (Error.of_string "Chunk size is too large"))
  | `Invalid_char ch ->
    raise_notrace (Fail (Error.create "Invalid chunk_length character" ch sexp_of_char))
;;

let version source =
  let version = version source in
  eol source;
  version
;;

let request source =
  let meth = meth source in
  let path = token source in
  let version = version source in
  let headers = Headers.of_rev_list (headers source) in
  Request.create ~version ~headers meth path
;;

let take len source =
  let available = Source.length source in
  let to_consume = min len available in
  if to_consume = 0 then raise_notrace Partial;
  let payload = Source.to_string source ~pos:0 ~len:to_consume in
  Source.advance source to_consume;
  payload
;;

type chunk_kind =
  | Start_chunk
  | Continue_chunk of int

type chunk_parser_result =
  | Chunk_complete of string
  | Done
  | Partial_chunk of string * int

let chunk chunk_kind source =
  match chunk_kind with
  | Start_chunk ->
    let chunk_length = chunk_length source in
    if chunk_length = 0
    then (
      eol source;
      Done)
    else (
      let current_chunk = take chunk_length source in
      let current_chunk_length = String.length current_chunk in
      if current_chunk_length = chunk_length
      then (
        eol source;
        Chunk_complete current_chunk)
      else Partial_chunk (current_chunk, chunk_length - current_chunk_length))
  | Continue_chunk len ->
    let chunk = take len source in
    let current_chunk_length = String.length chunk in
    if current_chunk_length = len
    then (
      eol source;
      Chunk_complete chunk)
    else Partial_chunk (chunk, len - current_chunk_length)
;;

type error =
  | Partial
  | Fail of Error.t

let run_parser ?pos ?len buf p =
  let pos = Option.value pos ~default:0 in
  let source = Source.of_bytes ~pos ?len buf in
  match p source with
  | exception Partial -> Error Partial
  | exception Fail m -> Error (Fail m)
  | v ->
    let consumed = source.pos - pos in
    Ok (v, consumed)
;;

let parse_request ?pos ?len buf = run_parser ?pos ?len buf request
let parse_chunk_length ?pos ?len buf = run_parser ?pos ?len buf chunk_length
let parse_chunk ?pos ?len buf chunk_kind = run_parser ?pos ?len buf (chunk chunk_kind)
