open Cli
open InfraredParser.Parser

let () =
  try
    InfraredShell.exec ()
  with
  | InfraredParsingError message ->
    print_endline message
  | _ ->
    print_endline "Infrared: Uncaught error occurred."
