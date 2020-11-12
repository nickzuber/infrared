open Ast

exception InfraredParsingError of int * string

let parse_source ~(file : string) ~(source : string) : program =
  let (ast, errs) = FlowParser.parse_source ~file ~source in
  FlowProgram (ast, errs)
