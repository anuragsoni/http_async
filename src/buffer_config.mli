type t [@@deriving sexp_of]

val create : ?initial_size:int -> ?max_buffer_size:int -> unit -> t
val initial_size : t -> int
val max_buffer_size : t -> int
