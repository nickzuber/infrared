open Ast

exception Infrared_parsing_error of int * string

let infrared_program_of_flow_program (flow_program: program) : program =
  let alpha_pass = Alpha.Transformer.transform flow_program in
  alpha_pass

let parse_source ~(file : string) ~(source : string) : program =
  try
    let flow_program = FlowParser.parse ~file ~source in
    (* This just prints the parsed FlowAST for reference while developing *)
    let flow_string = Printer.string_of_program flow_program in
    let _ = Printf.printf "%s\n\n" flow_string in
    infrared_program_of_flow_program flow_program
  with
  | FlowParser.Flow_parsing_error (count, message) ->
    raise (Infrared_parsing_error (count, message))
  | Alpha.Unhandled_parsing_step (message) ->
    raise (Infrared_parsing_error (1, message))
