open! Base

type t =
  { initial_size : int
  ; max_buffer_size : int
  }
[@@deriving sexp_of]

let validate t =
  if t.initial_size <= 0 || t.initial_size > t.max_buffer_size
  then raise_s [%sexp "Http_async.Buffer_config.validate: invalid config", { t : t }];
  t
;;

let create ?(initial_size = 16 * 1024) ?(max_buffer_size = Int.max_value) () =
  validate { initial_size; max_buffer_size }
;;

let initial_size t = t.initial_size
let max_buffer_size t = t.max_buffer_size
