open Core

type t = (string, string) List.Assoc.t [@@deriving sexp_of]

let empty = []
let pp fmt t = Sexp.pp fmt (sexp_of_t t)
let pp_hum fmt t = Sexp.pp_hum fmt (sexp_of_t t)
let of_list xs = xs
let to_list t = t
let find key t = List.Assoc.find ~equal:String.Caseless.equal t key

let add_unless_exists ~key ~value t =
  match find key t with
  | None -> (key, value) :: t
  | Some _ -> t
;;

let upsert ~key ~value t = List.Assoc.add ~equal:String.Caseless.equal t key value
