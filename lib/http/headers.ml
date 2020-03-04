open Base

let is_ascii t = String.for_all ~f:(fun c -> Char.to_int c <= 0x7F) t

module Header_key = struct
  module T = struct
    type t = String.Caseless.t [@@deriving sexp]

    let compare = String.Caseless.compare
  end

  include T
  include Comparable.Make (T)
end

type t = string list Map.M(Header_key).t [@@deriving sexp, compare]

let empty = Map.empty (module Header_key)
let of_list xs = Map.of_alist_multi (module Header_key) xs
let add key data t = Map.add_multi t ~key ~data
let find key t = Map.find t key

let add_if_missing key data t =
  match find key t with
  | None -> add key data t
  | Some _ -> t
;;

let to_alist t = Map.to_alist t
let remove key t = Map.remove t key
let pp fmt t = Sexp.pp fmt (sexp_of_t t)
let pp_hum fmt t = Sexp.pp_hum fmt (sexp_of_t t)
