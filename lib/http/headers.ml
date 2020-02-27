open Base

let is_ascii t = String.for_all ~f:(fun c -> Char.to_int c <= 0x7F) t

module Header_key = struct
  module T = struct
    type t = string [@@deriving sexp, compare]
  end

  include T
  include Comparable.Make (T)

  let of_string t =
    match is_ascii t with
    | true -> Ok (String.lowercase t)
    | false ->
      Error
        (Error.create
           "Header key needs to be ASCII"
           ("header_key", t)
           [%sexp_of: string * t])
  ;;

  let to_string t = t
end

module Header_value = struct
  type t = string [@@deriving sexp, compare]

  let of_string t =
    match is_ascii t with
    | true -> Ok t
    | false ->
      Error
        (Error.create
           "Header value needs to be ascii"
           ("header_value", t)
           [%sexp_of: string * t])
  ;;

  let to_string t = t
end

type t = Header_value.t list Map.M(Header_key).t [@@deriving sexp]

let empty = Map.empty (module Header_key)

let add key data t =
  match data with
  | [] -> Or_error.error_string "Header values can't be empty"
  | _ ->
    let open Or_error.Let_syntax in
    let%map header_name = Header_key.of_string key
    and header_values =
      Or_error.combine_errors (List.map ~f:Header_value.of_string data)
    in
    Map.update t header_name ~f:(fun _ -> header_values)
;;

let pp fmt t = Sexp.pp fmt (sexp_of_t t)
let pp_hum fmt t = Sexp.pp_hum fmt (sexp_of_t t)
