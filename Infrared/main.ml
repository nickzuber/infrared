open Cli
open InfraredUtils
open InfraredParser.Parser

let () =
  try
    InfraredShell.exec ()
  with
  | InfraredParsingError message ->
    Printf.printf "\n%s\n%s\n\n"
      ("Error: Found some basic errors while parsing" |> Chalk.bold |> Chalk.underline)
      message
  | _ ->
    print_endline "Infrared: Uncaught error occurred."
