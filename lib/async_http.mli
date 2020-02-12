open Core
open Async

module Headers : sig
  type t [@@deriving sexp_of]

  val empty : t
  val of_list : (string * string) list -> t
  val to_list : t -> (string * string) list
  val add_unless_exists : key:string -> value:string -> t -> t
  val upsert : key:string -> value:string -> t -> t
  val find : string -> t -> string option
  val pp : Format.formatter -> t -> unit
  val pp_hum : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]
end

module Body : sig
  type t =
    private
    [ `Empty
    | `String of string
    | `Bigstring of Bigstring.t
    | `Stream of Bigstring.t Core.Unix.IOVec.t Pipe.Reader.t
    ]
  [@@deriving sexp_of]

  val empty : t
  val of_string : string -> t
  val of_bigstring : Bigstring.t -> t
  val to_string_stream : t -> string Pipe.Reader.t
  val to_string : t -> string Deferred.t
  val drain : t -> unit Deferred.t
  val pp : Format.formatter -> t -> unit
  val pp_hum : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]
end

module Request : sig
  type meth = Httpaf.Method.t

  type t = private
    { target : string
    ; headers : Headers.t
    ; meth : meth
    ; body : Body.t
    }
  [@@deriving sexp_of, fields]

  val pp : Format.formatter -> t -> unit
  val pp_hum : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]
end

module Response : sig
  type status = Httpaf.Status.t

  type t = private
    { status : status
    ; headers : Headers.t
    ; body : Body.t
    }
  [@@deriving sexp_of, fields]

  val create : ?headers:Headers.t -> ?body:Body.t -> status -> t
  val pp : Format.formatter -> t -> unit
  val pp_hum : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]
  val of_string : ?headers:Headers.t -> ?status:status -> string -> t Deferred.t
  val of_bigstring : ?headers:Headers.t -> ?status:status -> Bigstring.t -> t Deferred.t

  (** Use [of_stream] to start a chunked http response. Use the {{:https://ocaml.janestreet.com/ocaml-core/latest/doc/async_kernel/Async_kernel/Pipe/index.html#writing} write} operations for an Async pipe. The stream
      is closed once the [unit Deferred.t] is determined. *)
  val of_stream
    :  ?headers:Headers.t
    -> ?status:status
    -> ?size:Int64.t
    -> (Bigstring.t Core.Unix.IOVec.t Pipe.Writer.t -> unit Deferred.t)
    -> t Deferred.t

  val of_file : ?headers:Headers.t -> ?status:status -> string -> t Deferred.t
end

module Server : sig
  type request_handler = Request.t -> Response.t Deferred.t

  val listen
    :  ?buffer_age_limit:Writer.buffer_age_limit
    -> ?max_connections:int
    -> ?max_accepts_per_batch:int
    -> ?backlog:int
    -> ?socket:([ `Unconnected ], ([< Async.Socket.Address.t ] as 'a)) Async.Socket.t
    -> on_handler_error:[ `Call of 'a -> exn -> unit | `Ignore | `Raise ]
    -> request_handler:request_handler
    -> error_handler:('a -> Httpaf.Server_connection.error_handler)
    -> ('a, 'b) Tcp.Where_to_listen.t
    -> ('a, 'b) Tcp.Server.t Deferred.t

  val listen_ssl
    :  ?buffer_age_limit:Writer.buffer_age_limit
    -> ?max_connections:int
    -> ?max_accepts_per_batch:int
    -> ?backlog:int
    -> ?socket:([ `Unconnected ], ([< Async.Socket.Address.t ] as 'a)) Async.Socket.t
    -> ?version:Async_ssl.Version.t
    -> ?options:Async_ssl.Opt.t list
    -> ?name:string
    -> ?allowed_ciphers:[ `Only of string list | `Openssl_default | `Secure ]
    -> ?ca_file:string
    -> ?ca_path:string
    -> ?verify_modes:Async_ssl.Verify_mode.t list
    -> on_handler_error:[ `Call of 'a -> exn -> unit | `Ignore | `Raise ]
    -> request_handler:request_handler
    -> error_handler:('a -> Httpaf.Server_connection.error_handler)
    -> crt_file:string
    -> key_file:string
    -> ('a, 'b) Tcp.Where_to_listen.t
    -> ('a, 'b) Tcp.Server.t Deferred.t
end

module Client : sig
  val request
    :  error_handler:Httpaf.Client_connection.error_handler
    -> meth:Httpaf.Method.standard
    -> ?headers:Httpaf.Headers.t
    -> ?buffer_age_limit:Writer.buffer_age_limit
    -> ?interrupt:unit Deferred.t
    -> ?reader_buffer_size:int
    -> ?writer_buffer_size:int
    -> ?version:Async_ssl.Version.t
    -> ?allowed_ciphers:[ `Only of string list | `Openssl_default | `Secure ]
    -> ?options:Async_ssl.Opt.t list
    -> ?verify_modes:Async_ssl.Verify_mode.t list
    -> ?ca_file:string
    -> ?ca_path:string
    -> ?verify:(Async_ssl.Ssl.Connection.t -> bool Deferred.t)
    -> Uri.t
    -> Response.t Deferred.t
end
