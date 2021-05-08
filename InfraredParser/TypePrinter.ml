open Ast
open InfraredUtils
open Flow_parser

type type_data = Loc.t * data_type
type type_mapping = (int, type_data) Hashtbl.t

let get_type tbl key : data_type =
  try Hashtbl.find tbl key
  with _ -> Unknown

let get_type_data tbl key : type_data option =
  try Some (Hashtbl.find tbl key)
  with _ -> None

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

  let create_type_mapping (program : program) : type_mapping =
    let type_map : type_mapping = Hashtbl.create 53 in
    let _ = match program with
      | TypedInfraredProgram (statements, env) ->
        let open TypedInfraredAst in
        List.iter (fun statement ->
            let (_, statement) = statement in
            match statement with
            | VariableDeclaration ((id_loc, id), _) ->
              let open Flow_parser.Loc in
              let line_number = id_loc.start.line in
              let d_type = get_type env id in
              Hashtbl.replace type_map line_number (id_loc, d_type);
              ()
            | _ -> ()
          ) statements
      | _ -> ()
    in
    type_map

  let pp_types_within_file ?padding:(padding=2) (file : string) (program : program) : unit =
    let type_map = create_type_mapping program in
    let padding = String.make padding ' ' in
    let file_underline = create_string "╍" (String.length file) in
    let source = Fs.read_file file in
    let lines = split source in
    let line_strs = List.mapi (fun i line ->
        let line_number = i + 1 in
        let type_data_maybe = get_type_data type_map line_number in
        let type_line = match type_data_maybe with
          | Some (loc, d_type) ->
            let open Flow_parser.Loc in
            let underline_spacing = String.make (loc.start.column + 1) ' ' in
            let underline = create_string "^" (loc._end.column - loc.start.column) in
            Printf.sprintf "\n%s%s  %s %s"
              (pad line_number)
              underline_spacing
              (underline |> Chalk.cyan |> Chalk.bold)
              ((Printer.string_of_data_type d_type) |> Chalk.cyan |> Chalk.bold)
          | None -> ""
        in
        Printf.sprintf "%s%s%s%s%s"
          (pad line_number)
          (Chalk.gray (string_of_int line_number))
          padding
          line
          type_line
      ) lines
    in
    Printf.printf "%s\n%s\n%s\n\n"
      (padding ^ (Chalk.bold file))
      (padding ^ (Chalk.bold file_underline))
      (String.concat "\n" line_strs)

end
