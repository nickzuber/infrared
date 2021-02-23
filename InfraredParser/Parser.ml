open Ast

exception InfraredParsingError of int * string

let parse_source ~(file : string) ~(source : string) : program =
  try
    let flow_program = FlowParser.parse ~file ~source in
    flow_program
  with
  | FlowParser.FlowParsingError (count, message) ->
    raise (InfraredParsingError (count, message))
