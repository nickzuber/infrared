open Cli

let () =
  try
    InfraredShell.exec ()
  with
  | Flow_parser.Parse_error.Error _ ->
    print_endline "Error while parsing."
  | _ ->
    print_endline "Unknown error occurred."
