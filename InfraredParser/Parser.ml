open Ast
open InfraredUtils

exception Infrared_parsing_error of int * string

let infrared_program_of_flow_program (flow_program: program) : program =
  let alpha_pass = Alpha.StatementTransformer.transform flow_program in
  alpha_pass

let flow_program_of_file ~(file : string) ~(source : string) : program =
  let flow_program = FlowParser.parse ~file ~source in
  flow_program

let parse_source ~(file : string) ~(source : string) : program =
  try
    let flow_program = flow_program_of_file ~file ~source in
    let _ = if Settings.debug_mode then
        begin
          (* This just prints the parsed FlowAST for reference while developing *)
          let _ = Printer.pprint_program_with_title "Flow Ast" flow_program in
          ()
        end
    in
    infrared_program_of_flow_program flow_program
  with
  | FlowParser.Flow_parsing_error (count, message) ->
    raise (Infrared_parsing_error (count, message))
  | Alpha.Unhandled_parsing_step (message) ->
    raise (Infrared_parsing_error (1, message))
