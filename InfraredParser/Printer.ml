module Ast = Flow_parser.Ast
module Loc = Flow_parser.Loc
module Err = Flow_parser.Parse_error
module E = Ast.Expression
module P = Ast.Pattern
module S = Ast.Statement
module L = Ast.Literal
module SL = Ast.StringLiteral
module T = Ast.Type
module V = Ast.Variance
module C = Ast.Class
module F = Ast.Function
module J = Ast.JSX

let string_of_ast (ast : Loc.t Ast.program * (Loc.t * Err.t) list) : string =
  let (program, _err) = ast in
  match program with
  | (loc, _stmts, _comments) ->
    Printf.sprintf "%d, %d, %d"
      loc.start.line loc.start.column loc.start.offset
