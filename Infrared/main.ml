open Cli
open InfraredUtils
open InfraredParser.Parser

let () =
  try
    InfraredShell.exec ()
  with
  | Infrared_parsing_error (count, message) ->
    let summary = Printf.sprintf "Failed w/ %n error(s) found" count in
    Printf.printf "\n%s\n%s\n\n"
      (summary |> Chalk.red |> Chalk.bold)
      message
  | _ ->
    print_endline "Infrared: Uncaught error occurred."
