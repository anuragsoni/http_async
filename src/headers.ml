open Core
include Common_header_names

type t = (string, string) List.Assoc.t [@@deriving sexp]

let of_rev_list xs = xs
let of_list xs = List.rev xs
let iter t ~f = List.iter t ~f:(fun (key, data) -> f ~key ~data)
let mem t key = List.Assoc.mem ~equal:String.Caseless.equal t key
let find t key = List.Assoc.find t key ~equal:String.Caseless.equal

let find_multi t key =
  let rec aux acc = function
    | [] -> List.rev acc
    | (k, v) :: xs when String.Caseless.equal k key -> aux (v :: acc) xs
    | _ :: xs -> aux acc xs
  in
  aux [] t
;;

let empty = []
let add_unless_exists t ~key ~data = if not (mem t key) then (key, data) :: t else t
