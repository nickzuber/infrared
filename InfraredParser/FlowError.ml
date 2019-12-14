module FlowError = struct
  let interleave_error_within_file file err : string =
    let open Batteries in
    let _error_string = Flow_parser.Parse_error.PP.error err in
    let filelines = File.lines_of file in
    let _ = Enum.iter (fun line ->
        print_endline line
      ) filelines
    in ""

  let string_of_error file err : string =
    let (loc, err) = err in
    let location_string = Flow_parser.Loc.to_string loc in
    Printf.sprintf "%s: %s"
      (location_string)
      (interleave_error_within_file file err)

  let string_of_errors file errs : string =
    let prefix = " - " in
    let errors_string =
      errs
      |> List.map (fun err -> string_of_error file err)
      |> String.concat ("\n" ^ prefix)
    in
    prefix ^ errors_string ^ "\n"
end
