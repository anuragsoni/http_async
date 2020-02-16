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

let%expect_test "Headers test" =
  let print_header condition hs = printf !"%s - %{sexp: t}\n" condition hs in
  print_header "empty headers" empty;
  [%expect {| empty headers - () |}];
  print_header "convert from list" (of_list [ "Hello", "World"; "Foo", "bar" ]);
  [%expect {| convert from list - ((Hello World) (Foo bar)) |}];
  print_header "add item to headers" (add_unless_exists ~key:"Foo" ~value:"Bar" empty);
  [%expect {| add item to headers - ((Foo Bar)) |}];
  print_header
    "trying to add same key with different case doesn't change the list"
    (add_unless_exists ~key:"Foo" ~value:"Bar" (of_list [ "foo", "bar"; "baz", "foo" ]));
  [%expect
    {| trying to add same key with different case doesn't change the list - ((foo bar) (baz foo)) |}];
  print_header
    "upsert on a key with different value replaces the entry in the list"
    (upsert ~key:"Foo" ~value:"123" (of_list [ "AA", "11"; "foo", "bar" ]));
  [%expect
    {| upsert on a key with different value replaces the entry in the list - ((Foo 123) (AA 11)) |}]
;;
