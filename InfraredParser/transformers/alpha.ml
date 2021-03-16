open Ast
module FlowAst = Flow_parser.Ast
module Loc = Flow_parser.Loc
open VariableDeclarationTransformer
open BinaryTransformer

exception Illegal_parsing_step of string

let rec transform (prog : program) : program =
  let flow_program = match prog with
    | FlowProgram ((_loc, flow_statements, _comments), _errs) ->
      let statements = infrared_statements_of_flow_statements flow_statements in
      InfraredProgram (statements)
    | _ -> raise
             (Illegal_parsing_step
                "Expected FlowProgram at VariableDeclarationTransformer#transforming")
  in
  flow_program

and infrared_statements_of_flow_statements (flow_statements : Loc.t FlowAst.Statement.t list)
  : InfraredAst.statement list =
  let infrared_statements = List.map
      (fun statement -> infrared_statement_of_flow_statement statement)
      flow_statements
  in
  infrared_statements

and infrared_statement_of_flow_statement (flow_statement : Loc.t FlowAst.Statement.t)
  : InfraredAst.statement =
  let open FlowAst.Statement in
  let open InfraredAst in
  let (_loc, statement) = flow_statement in
  match statement with
  | VariableDeclaration obj -> VariableDeclarationTransformer.transform obj
  | Expression expression_object ->
    let open FlowAst.Expression in
    let (_loc, expr) = expression_object.expression in
    (
      match expr with
      | Binary binary_expression -> BinaryTransformer.transform binary_expression
      | _ -> Expression Null
    )
  | _ -> Expression Null
