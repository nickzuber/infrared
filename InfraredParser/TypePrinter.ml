open Ast
open InfraredUtils
open Flow_parser

type type_mapping = (int, Loc.t * data_type) Hashtbl.t

module TypePrinter = struct
  let split = Str.split (Str.regexp "\n")

  let pad (n : int) = match n with
    | _ when n < 10 -> "  "
    | _ when n < 100 -> " "
    | _ -> ""

  let create_string (char : string) (length : int) : string =
    Array.make length char
    |> Array.to_list
    |> String.concat ""

  let create_type_mapping = ()

  let pp_types_within_file ?padding:(padding=2) (file : string) (_program : program) (_env : environment) : unit =
    let padding = String.make padding ' ' in
    let file_underline = create_string "╍" (String.length file) in
    let source = Fs.read_file file in
    let lines = split source in
    let line_strs = List.mapi (fun i line ->
        let line_number = i + 1 in
        Printf.sprintf "%s%s%s%s"
          (pad line_number)
          (Chalk.gray (string_of_int line_number))
          padding
          line
      ) lines
    in
    Printf.printf "%s\n%s\n%s\n\n"
      (padding ^ (Chalk.bold file))
      (padding ^ (Chalk.bold file_underline))
      (String.concat "\n" line_strs)

end
