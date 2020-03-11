open Core
open Async

module Headers : sig
  type t [@@deriving sexp]

  val empty : t
  val of_list : (string * string) list -> t
  val add : string -> string -> t -> t
  val find : string -> t -> string list option
  val add_if_missing : string -> string -> t -> t
  val remove : string -> t -> t
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
  val to_string_pipe : t -> string Pipe.Reader.t
  val empty : t
  val of_string : string -> t
  val of_bigstring : Bigstring.t -> t
  val of_pipe : ?length:Int64.t -> iovec Pipe.Reader.t -> t
  val of_string_pipe : ?length:Int64.t -> string Pipe.Reader.t -> t
end

module Request : sig
  type t = private
    { target : string
    ; state : Univ_map.t
    ; headers : Headers.t
    ; body : Body.t
    ; meth : Httpaf.Method.t
    }
  [@@deriving sexp_of, fields]

  val make : ?headers:Headers.t -> ?body:Body.t -> Httpaf.Method.t -> string -> t
end

module Response : sig
  type t =
    { status : Httpaf.Status.t
    ; headers : Headers.t
    ; body : Body.t
    ; state : Univ_map.t
    }
  [@@deriving sexp_of]

  val make : ?headers:Headers.t -> ?body:Body.t -> Httpaf.Status.t -> t
  val of_string : ?headers:Headers.t -> ?status:Httpaf.Status.t -> string -> t Deferred.t

  val of_bigstring
    :  ?headers:Headers.t
    -> ?status:Httpaf.Status.t
    -> Bigstring.t
    -> t Deferred.t

  val of_file
    :  ?headers:Headers.t
    -> ?status:Httpaf.Status.t
    -> Filename.t
    -> t Deferred.t
end

module Service : sig
  type t = Request.t -> Response.t Deferred.t
end

module Server : sig
  val create_connection_handler
    :  Service.t
    -> ?config:Httpaf.Config.t
    -> ?error_handler:Httpaf.Server_connection.error_handler
    -> 'a
    -> Reader.t
    -> Writer.t
    -> unit Deferred.t
end

module Client : sig
  val request
    :  ?ssl_options:Async_connection.Client.ssl_options
    -> ?headers:Headers.t
    -> ?body:Body.t
    -> Httpaf.Method.standard
    -> Uri.t
    -> Response.t Deferred.Or_error.t

  val get
    :  ?ssl_options:Async_connection.Client.ssl_options
    -> ?headers:Headers.t
    -> Uri.t
    -> Response.t Deferred.Or_error.t

  val post
    :  ?ssl_options:Async_connection.Client.ssl_options
    -> ?headers:Headers.t
    -> ?body:Body.t
    -> Uri.t
    -> Response.t Deferred.Or_error.t
end
