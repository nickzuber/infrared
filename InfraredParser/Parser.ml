open Ast

exception InfraredParsingError of int * string

let infrared_program_of_flow_program (flow_program: program) : program =
  let alpha_pass = Alpha.transform flow_program in
  alpha_pass

let parse_source ~(file : string) ~(source : string) : program =
  try
    let flow_program = FlowParser.parse ~file ~source in
    let flow_string = Printer.string_of_program flow_program in
    let _ = Printf.printf "%s\n\n" flow_string in
    infrared_program_of_flow_program flow_program
  with
  | FlowParser.FlowParsingError (count, message) ->
    raise (InfraredParsingError (count, message))
