open InfraredParser.Ast
open InfraredParser
open InfraredUtils

let print_program (title : string) (program : program) : unit =
  Printf.printf "%s %s\n\n"
    (("[" ^ title ^ "]") |> Chalk.white |> Chalk.bold)
    (Printer.string_of_program program)

let assign_types ~(file : string) ~(program : program) =
  let env : environment = Hashtbl.create 53 in
  let _ = file in
  program
  |> Remove_assignify.transform
  |> Uniquify.transform
  |> Hoisify.transform env
  |> Typify.transform env
  |> Function_returns_refinement.transform env
  |> Realign_typed_expressions.transform env
(* inner_function_refinement - function args get refined based on use within function *)
(* global_refinement - global refinement for all vars and usages *)

let assign_types_with_debugging ~(file : string) ~(program : program) =
  let env : environment = Hashtbl.create 53 in
  Printf.printf "%s\n%s\n"
    (Printer.string_of_title "Original program")
    (Fs.read_file file);
  program |> Printer.pprint_program_with_title "Converted program"
  |> Remove_assignify.transform |> Printer.pprint_program_with_title "Transform assignments into declarations"
  |> Uniquify.transform |> Printer.pprint_program_with_title "Uniqufy variable names (removes shadowing)"
  |> (Hoisify.transform env) |> Printer.pprint_program_with_title "Hoist function declarations within closures"
  |> (Typify.transform env) |> Printer.pprint_program_with_title "Assign base types"
  |> (Function_returns_refinement.transform env) |> Printer.pprint_program_with_title "Assign Functions their return types"
  |> (Realign_typed_expressions.transform env) |> Printer.pprint_program_with_title "Re-align typed expressions"

let assign_types ~(file : string) ~(program : program) =
  if Settings.debug_mode then
    assign_types_with_debugging ~file ~program
  else
    assign_types ~file ~program
