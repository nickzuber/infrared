open Ast
module FlowAst = Flow_parser.Ast
module Loc = Flow_parser.Loc
module Err = Flow_parser.Parse_error

let string_of_ast (ast : Loc.t FlowAst.program * (Loc.t * Err.t) list) : string =
  let flow_ast = FlowPrinter.string_of_ast ast in
  flow_ast

let string_of_program (prog: program) : string =
  match prog with
  | FlowProgram (ast, errs) -> FlowPrinter.string_of_ast (ast, errs)
  | _ -> "#<unhandled program>"
