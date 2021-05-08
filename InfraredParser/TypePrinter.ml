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

let rec walk_statement (statement : TypedInfraredAst.statement) (env : environment) (type_map : type_mapping) : unit =
  let open TypedInfraredAst in
  let open Flow_parser.Loc in
  let (statement_loc, statement) = statement in
  match statement with
  | VariableDeclaration ((id_loc, id), _) ->
    let line_number = id_loc.start.line in
    let d_type = get_type env id in
    Hashtbl.replace type_map line_number (id_loc, d_type);
    ()
  | FunctionDeclaration ((name_loc, name), _args, body) ->
    let line_number = name_loc.start.line in
    let d_type = get_type env name in
    Hashtbl.replace type_map line_number (name_loc, d_type);
    List.iter (fun statement -> walk_statement statement env type_map) body
  | If (_expr, s1, s2) ->
    walk_statement s1 env type_map;
    walk_statement s2 env type_map
  | Return (d_type, _expr) ->
    let line_number = statement_loc.start.line in
    Hashtbl.replace type_map line_number (statement_loc, d_type);
  | Block body -> List.iter (fun statement -> walk_statement statement env type_map) body
  | _ -> ()

module TypePrinter = struct
  let split = Str.split (Str.regexp "\n")

  let pad (n : int) = match n with
    | _ when n < 10 -> "  "
    | _ when n < 100 -> " "
    | _ -> ""

  let pad_of_number (n : int) = match n with
    | _ when n < 10 -> "  "
    | _ when n < 100 -> "   "
    | _ when n < 1000 -> "    "
    | _ -> ""

  let create_string (char : string) (length : int) : string =
    Array.make length char
    |> Array.to_list
    |> String.concat ""

  let create_type_mapping (program : program) : type_mapping =
    let type_map : type_mapping = Hashtbl.create 53 in
    let _ = match program with
      | TypedInfraredProgram (statements, env) ->
        List.iter (fun statement -> walk_statement statement env type_map) statements
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
            let underline =
              (create_string "▀" (loc._end.column - loc.start.column)) ^
              "▏"
            in
            Printf.sprintf "\n%s%s%s%s %s"
              (pad line_number)
              (pad_of_number line_number)
              underline_spacing
              (underline |> Chalk.white |> Chalk.bold)
              ((Printer.string_of_data_type d_type) |> Chalk.white |> Chalk.bold)
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
