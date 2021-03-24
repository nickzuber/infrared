open Ast
open InfraredUtils
module FlowAst = Flow_parser.Ast
module Loc = Flow_parser.Loc
module Err = Flow_parser.Parse_error

let mkstr = Printf.sprintf

let rec string_of_infrared_statement (statement: InfraredAst.statement) : string =
  let open InfraredAst in
  match statement with
  | VariableDeclaration (id, value) ->
    mkstr "%s %s <- %s"
      (Chalk.yellow "[VariableDeclaration]")
      id
      (string_of_infrared_expression value)
  | FunctionDeclaration (name, params, body) ->
    let params' = String.concat ", " params in
    let body' = String.concat "\n\t" (List.map string_of_infrared_statement body) in
    Printf.sprintf "%s %s (%s) {\n\t%s\n}"
      (Chalk.yellow "[FunctionDeclaration]")
      name
      (params')
      (body')
  | Expression expr ->
    (Chalk.yellow "[Expression] ") ^
    (string_of_infrared_expression expr)
  | _ -> "#<unhandled_statement>"

and string_of_infrared_expression (expression : InfraredAst.expression) : string =
  let open InfraredAst in
  match expression with
  | Variable id -> id
  | String s -> s
  | Number n -> string_of_int n
  | Boolean b -> string_of_bool b
  | Null -> "null"
  | Undefined -> "undefined"
  | BinaryOperation (binop, left, right) ->
    Printf.sprintf "%s %s %s"
      (string_of_infrared_expression left)
      (string_of_infrared_binop binop)
      (string_of_infrared_expression right)
  | _ -> "#<unhandled_expression>"

and string_of_infrared_binop (binop : InfraredAst.binop) : string =
  let open InfraredAst in
  match binop with
  | Plus -> "+"
  | Minus -> "-"
  | Multiply -> "*"
  | Divide -> "/"
  | Exponent -> "**"
  | Modulo -> "%"
  | LeftShift -> ">>"
  | RightShift -> "<<"
  | BitOr -> "|"
  | BitXor -> "^"
  | BitAnd -> "&"
  | And -> "&&"
  | Or -> "||"
  | In -> "in"
  | InstanceOf -> "instanceof"
  | Compare cmp ->
    (
      match cmp with
      | Equal -> "=="
      | NotEqual -> "!="
      | GreaterThan -> ">"
      | LessThan -> "<"
    )


let string_of_infrared_ast (statements : InfraredAst.statement list) : string =
  let statement_strings = List.map string_of_infrared_statement statements in
  let joined_statement_strings = String.concat "\n" statement_strings in
  "\n" ^ joined_statement_strings

let string_of_ast (ast : Loc.t FlowAst.program * (Loc.t * Err.t) list) : string =
  let flow_ast = FlowPrinter.string_of_ast ast in
  flow_ast

let string_of_program (prog: program) : string =
  match prog with
  | FlowProgram (ast, errs) -> FlowPrinter.string_of_ast (ast, errs)
  | InfraredProgram (statements) -> string_of_infrared_ast statements
  | TypedInfraredProgram (statements, _env) -> string_of_infrared_ast statements
