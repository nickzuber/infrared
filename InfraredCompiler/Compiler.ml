open InfraredParser.Ast
open InfraredParser
open InfraredUtils

let print_program (title : string) (program : program) : unit =
  Printf.printf "%s %s\n\n"
    (("[" ^ title ^ "]") |> Chalk.white |> Chalk.bold)
    (Printer.string_of_program program)

let assign_types_without_debugging ~(file : string) ~(program : program) =
  let _ = file in
  program
  |> Remove_assignify.transform
  |> Uniquify.transform
  |> Typify.transform

let assign_types_with_debugging ~(file : string) ~(program : program) =
  let _ = file in
  program |> Printer.pprint_program_with_title "Initial program"
  |> Remove_assignify.transform |> Printer.pprint_program_with_title "Transform assignments into declarations"
  |> Uniquify.transform |> Printer.pprint_program_with_title "Uniqufy variable names (removes closures)"
  |> Typify.transform |> Printer.pprint_program_with_title "Type the program"

let assign_types ~(file : string) ~(program : program) =
  if Settings.debug_mode then
    assign_types_with_debugging ~file ~program
  else
    assign_types_without_debugging ~file ~program
