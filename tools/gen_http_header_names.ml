open! Core
open Ppxlib
module Builder = Ast_builder.Default

module Csv_line = struct
  type t = { name : string }

  let lines_from_file name =
    In_channel.with_file name ~f:(fun chan ->
      let (_ : string) = In_channel.input_line_exn chan in
      In_channel.fold_lines chan ~init:[] ~f:(fun acc line ->
        match String.split line ~on:',' with
        | name :: _ -> { name } :: acc
        | _ -> invalid_arg "Invaline line"))
  ;;
end

let header_name_to_fn_name name =
  String.map name ~f:(function
    | '-' -> '_'
    | ch -> Char.lowercase ch)
;;

let codegen_ml csv_lines =
  let to_ast { Csv_line.name } =
    let loc = Location.none in
    let fn_name =
      Builder.ppat_var ~loc (Builder.Located.mk ~loc (header_name_to_fn_name name))
    in
    let name = Builder.estring ~loc name in
    [%stri let [%p fn_name] = [%e name]]
  in
  List.iter csv_lines ~f:(fun line ->
    let ast = to_ast line in
    Format.printf "%a\n@." Pprintast.structure_item ast)
;;

let codegen_mli csv_lines =
  let to_ast { Csv_line.name } =
    let loc = Location.none in
    let fn_name =
      Ast_helper.Val.mk
        { Location.txt = header_name_to_fn_name name; loc = Location.none }
        [%type: string]
    in
    Ast_helper.Sig.value fn_name
  in
  List.iter csv_lines ~f:(fun line ->
    let ast = to_ast line in
    Format.printf "%a\n@." Pprintast.signature_item ast)
;;

let codegen filename mli =
  let csv_lines =
    filename
    |> Csv_line.lines_from_file
    |> List.filter ~f:(fun { name } -> String.(name <> "*") && String.(name <> "If"))
  in
  if mli then codegen_mli csv_lines else codegen_ml csv_lines
;;

let command =
  Command.basic
    ~summary:"Generate list of header names"
    Command.Let_syntax.(
      let%map_open filename = anon ("filename" %: Filename_unix.arg_type)
      and mli = flag "-mli" no_arg ~doc:" Generate mli" in
      fun () -> codegen filename mli)
;;

let () = Command_unix.run command
