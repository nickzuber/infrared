open InfraredParser.Ast
open InfraredParser
open InfraredUtils
open TypePrinter

let print_program (title : string) (program : program) : unit =
  Printf.printf "%s %s\n\n"
    (("[" ^ title ^ "]") |> Chalk.white |> Chalk.bold)
    (Printer.string_of_program program)

let assign_types ~(file : string) ~(program : program) : program =
  let env : environment = Hashtbl.create 53 in
  let _ = file in
  let typed_program =
    program
    |> Remove_assignify.transform
    |> Uniquify.transform
    |> Initialize_function_declarations.transform env
    |> Typify.transform env
    |> Function_returns_refinement.transform env
    |> Realign_typed_expressions.transform env
  in
  typed_program

(* inner_function_refinement - function args get refined based on use within function *)
(* global_refinement - global refinement for all vars and usages *)

let assign_types_with_debugging ~(file : string) ~(program : program) : program =
  let env : environment = Hashtbl.create 53 in
  Printf.printf "%s\n%s\n"
    (Printer.string_of_title "Original program")
    (Fs.read_file file);
  let typed_program =
    program |> Printer.pprint_program_with_title "Converted program"
    |> Remove_assignify.transform |> Printer.pprint_program_with_title "Transform assignments into declarations"
    |> Uniquify.transform |> Printer.pprint_program_with_title "Uniqufy variable names (removes shadowing)"
    |> (Initialize_function_declarations.transform env) |> Printer.pprint_program_with_title "Initialize function declarations within closures"
    |> (Typify.transform env) |> Printer.pprint_program_with_title "Assign base types"
    |> (Function_returns_refinement.transform env) |> Printer.pprint_program_with_title "Assign Functions their return types"
    |> (Realign_typed_expressions.transform env) |> Printer.pprint_program_with_title "Re-align typed expressions"
  in
  typed_program

let assign_types ~(file : string) ~(program : program) =
  let typed_program = if !Settings.debug_mode then
      assign_types_with_debugging ~file ~program
    else
      assign_types ~file ~program
  in
  if !Settings.show_types_in_console then
    TypePrinter.pp_types_within_file file typed_program;
  typed_program
