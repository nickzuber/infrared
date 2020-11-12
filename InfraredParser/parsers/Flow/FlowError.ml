open InfraredUtils

module FlowError = struct
  let split = Str.split (Str.regexp "\n")
  let pad (n : int) = match n with
    | _ when n < 10 -> "  "
    | _ when n < 100 -> " "
    | _ -> ""
  let s (n : int) = match n with
    | _ when n < 10 -> " "
    | _ when n < 100 -> "  "
    | _ when n < 1000 -> "   "
    | _ -> ""

  let interleave_error_within_file ~padding file err : string =
    let open Flow_parser.Loc in
    let padding = String.make padding ' ' in
    (* The amount of lines we show above and below the error lines. *)
    let preview_length = 0 in
    let (loc, err) = err in
    let error_string = Flow_parser.Parse_error.PP.error err in
    let source = Fs.read_file file in
    (* Lines of file to print *)
    let lines = split source in
    let line_start = max (loc.start.line - preview_length) 1 in
    let line_end = loc._end.line + preview_length in
    let preview_string = ref "" in
    let _ = List.iteri (fun i line ->
        let i = i + 1 in
        match i with
        | _ when i = loc.start.line ->
          let spacing = String.make (loc.start.column + 1) ' ' in
          let underline = String.make (loc._end.column - loc.start.column) '^' in
          let line_string = Printf.sprintf "%s%s\n%s%s"
              padding
              line
              (Chalk.gray ((s i) ^ (pad i) ^ padding ^ "   "))
              (spacing ^ padding ^ (underline |> Chalk.red |> Chalk.bold))
          in
          let str = Printf.sprintf "%s%s %s%s| %s"
              padding
              (">" |> Chalk.bold |> Chalk.red)
              (string_of_int i)
              (pad i)
              line_string
          in
          preview_string := !preview_string ^ "\n" ^ str
        | _ when i > line_end -> ()
        | _ when i >= line_start ->
          let line_string = line in
          let str =
            Printf.sprintf "%s  %s%s| %s"
              padding
              (string_of_int i)
              (pad i)
              line_string
          in
          preview_string := !preview_string ^ "\n" ^ (Chalk.gray str)
        | _ -> ()
      ) lines
    in
    Printf.sprintf "\n%s%s %s%s.%s"
      padding
      (Chalk.bold "Error")
      (Chalk.white "(parsing): ")
      (Chalk.white error_string)
      !preview_string

  let string_of_errors_in_file file errs : string =
    let errors_string =
      errs
      |> List.map (fun err -> interleave_error_within_file ~padding:8 file err)
      |> String.concat ("")
    in
    errors_string ^ "\n"

  let string_of_error err : string =
    let (loc, err) = err in
    let location_string = Flow_parser.Loc.to_string loc in
    let error_string = Flow_parser.Parse_error.PP.error err in
    Printf.sprintf "%s: %s"
      location_string
      error_string

  let string_of_errors errs : string =
    let prefix = " - " in
    let errors_string =
      errs
      |> List.map (fun err -> string_of_error err)
      |> String.concat ("\n" ^ prefix)
    in
    prefix ^ errors_string ^ "\n"
end
