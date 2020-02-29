open Core
open Async

val log_src : Logs.src

module Headers : sig
  module Header_key : sig
    type t [@@deriving sexp, compare]

    val to_string : t -> string
    val of_string : string -> t Or_error.t
    val of_string_exn : string -> t
  end

  module Header_value : sig
    type t [@@deriving sexp, compare]

    val to_string : t -> string
    val of_string : string -> t Or_error.t
    val of_string_exn : string -> t
  end

  type t [@@deriving sexp]

  val empty : t
  val of_list : (Header_key.t * Header_value.t) list -> t
  val add : Header_key.t -> Header_value.t -> t -> t
  val find : Header_key.t -> t -> Header_value.t list option
  val add_if_missing : Header_key.t -> Header_value.t -> t -> t
  val remove : Header_key.t -> t -> t
  val content_length : Int64.t -> Header_key.t * Header_value.t
  val pp : Format.formatter -> t -> unit
  val pp_hum : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]
end

module Body : sig
  type iovec = Bigstring.t Core.Unix.IOVec.t [@@deriving sexp_of]

  type content = private
    | Empty
    | String of string
    | Bigstring of bigstring
    | Stream of iovec Pipe.Reader.t
  [@@deriving sexp_of]

  type t = private
    { length : Int64.t option
    ; content : content
    }
  [@@deriving sexp_of, fields]

  val drain : t -> unit Deferred.t
  val to_string : t -> string Deferred.t
  val to_pipe : t -> iovec Pipe.Reader.t
  val empty : t
  val of_string : string -> t
  val of_bigstring : Bigstring.t -> t
  val of_stream : ?length:Int64.t -> iovec Pipe.Reader.t -> t
end

module Server : sig
  val create_connection_handler
    :  ?config:Httpaf.Config.t
    -> request_handler:('a -> Httpaf.Server_connection.request_handler)
    -> ?error_handler:Httpaf.Server_connection.error_handler
    -> 'a
    -> Reader.t
    -> Writer.t
    -> unit Deferred.t
end

module Client : sig
  val request
    :  ?ssl_options:Async_connection.Client.ssl_options
    -> ?headers:Httpaf.Headers.t
    -> ?body:Body.t
    -> Httpaf.Method.standard
    -> Uri.t
    -> (Httpaf.Response.t * Body.t) Deferred.Or_error.t

  val get
    :  ?ssl_options:Async_connection.Client.ssl_options
    -> ?headers:Httpaf.Headers.t
    -> Uri.t
    -> (Httpaf.Response.t * Body.t) Deferred.Or_error.t

  val post
    :  ?ssl_options:Async_connection.Client.ssl_options
    -> ?headers:Httpaf.Headers.t
    -> ?body:Body.t
    -> Uri.t
    -> (Httpaf.Response.t * Body.t) Deferred.Or_error.t
end
