open Ast
open InfraredUtils
module FlowAst = Flow_parser.Ast
module Loc = Flow_parser.Loc
module Err = Flow_parser.Parse_error

let string_of_type (t : string) : string =
  Printf.sprintf "[%s] "
    (Chalk.green t)

let get_type tbl key =
  Hashtbl.find tbl key

let rec string_of_infrared_statement (statement: InfraredAst.statement) : string =
  let open InfraredAst in
  match statement with
  | VariableDeclaration (id, value) ->
    Printf.sprintf "%s %s <- %s"
      (string_of_type "VariableDeclaration")
      id
      (string_of_infrared_expression value)
  | FunctionDeclaration (name, params, body) ->
    let params' = String.concat ", " params in
    let body' = String.concat "\n\t" (List.map string_of_infrared_statement body) in
    Printf.sprintf "%s %s (%s) {\n\t%s\n}"
      (string_of_type "FunctionDeclaration")
      name
      (params')
      (body')
  | Return expr ->
    Printf.sprintf "%sreturn %s"
      (string_of_type "Return")
      (string_of_infrared_expression expr)
  | Expression expr ->
    (string_of_type "Expression") ^
    (string_of_infrared_expression expr)
  | _ -> string_of_type "#<unhandled_statement>"

and string_of_infrared_expression (expression : InfraredAst.expression) : string =
  let open InfraredAst in
  match expression with
  | Variable id -> id
  | String s -> "\"" ^ s ^ "\""
  | Number n -> string_of_int n
  | Boolean b -> string_of_bool b
  | Null -> "null"
  | Undefined -> "undefined"
  | BinaryOperation (binop, left, right) ->
    Printf.sprintf "%s %s %s"
      (string_of_infrared_expression left)
      (string_of_infrared_binop binop)
      (string_of_infrared_expression right)
  | Assignment (id, expr) ->
    Printf.sprintf "%s = %s"
      (id)
      (string_of_infrared_expression expr)
  | Object pairs ->
    let pairs_strs = List.map (fun pair ->
        let (key, value) = pair in
        let value_str = string_of_infrared_expression value in
        Printf.sprintf "\n\t%s: %s"
          key value_str
      ) pairs in
    let formatted_pairs = String.concat "," pairs_strs in
    Printf.sprintf "{%s\n}" formatted_pairs
  | Access (e1, e2) ->
    Printf.sprintf "(%s%s)"
      (string_of_infrared_expression e1)
      (string_of_infrared_property e2)
  | _ -> string_of_type "#<unhandled_expression>"

and string_of_infrared_property (prop : InfraredAst.property) : string =
  let open InfraredAst in
  match prop with
  | PropertyExpression expr -> "[" ^ (string_of_infrared_expression expr) ^ "]"
  | PropertyIdentifier id -> "." ^ id

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

let rec string_of_primative (prim : primative_data_type) : string =
  match prim with
  | Null -> "Null"
  | Undefined -> "Undefined"
  | String -> "String"
  | Boolean -> "Boolean"
  | Number -> "Number"
  | Object pairs ->
    let pp_pairs = List.map (fun pair ->
        let (key, d_type) = pair in
        Printf.sprintf "\n\t%s: %s"
          key
          (string_of_data_type d_type)
      ) pairs
    in
    Printf.sprintf "Object {%s\n}"
      (String.concat "; " pp_pairs)
  | Array _ -> "Array"
  | Function _ -> "Function"

and string_of_data_type (d_type : data_type) : string =
  match d_type with
  | Generic tag -> "'" ^ tag
  | Defer _expr -> "Defer"
  | Primative prim -> string_of_primative prim
  | Reduction d_types ->
    let d_types_str = List.map string_of_data_type d_types in
    let str = String.concat ", " d_types_str in
    Printf.sprintf "ψ(%s)"
      str
  | Drill (d_type, prop) ->
    Printf.sprintf "Drill(%s, %s)"
      (string_of_data_type d_type)
      (string_of_infrared_property prop)

let pp_string_of_data_type (d_type : data_type) : string =
  let d_type_str = string_of_data_type d_type in
  let str = "⊢ " ^ d_type_str in
  str |> Chalk.white |> Chalk.bold

let rec string_of_typed_infrared_statement (statement: TypedInfraredAst.statement) (env : environment) : string =
  let open TypedInfraredAst in
  match statement with
  | VariableDeclaration (id, typed_value) ->
    let (d_type, value) = typed_value in
    Printf.sprintf "%s %s <- %s %s"
      (string_of_type "VariableDeclaration")
      id
      (string_of_infrared_expression value)
      (pp_string_of_data_type d_type)
  | FunctionDeclaration (name, params, body) ->
    let typed_params = List.map (fun param ->
        let d_type = get_type env param in
        (d_type, param)
      ) params
    in
    let formatted_typed_params = List.map (fun typed_param ->
        let (d_type, param) = typed_param in
        Printf.sprintf "%s %s"
          param
          (pp_string_of_data_type d_type)
      ) typed_params
    in
    let params' = String.concat ", " formatted_typed_params in
    let body' = String.concat "\n\t" (List.map (fun s -> string_of_typed_infrared_statement s env) body) in
    Printf.sprintf "%s %s (%s) {\n\t%s\n}"
      (string_of_type "FunctionDeclaration")
      name
      (params')
      (body')
  | Return expr ->
    let (d_type, expr) = expr in
    Printf.sprintf "%sreturn %s %s"
      (string_of_type "Return")
      (string_of_infrared_expression expr)
      (pp_string_of_data_type d_type)
  | Expression typed_expr ->
    let (d_type, expr) = typed_expr in
    Printf.sprintf "%s %s %s"
      (string_of_type "Expression")
      (string_of_infrared_expression expr)
      (pp_string_of_data_type d_type)
  | _ -> string_of_type "#<unhandled_statement>"

let string_of_infrared_ast (statements : InfraredAst.statement list) : string =
  let statement_strings = List.map string_of_infrared_statement statements in
  let joined_statement_strings = String.concat "\n" statement_strings in
  "\n" ^ joined_statement_strings

let string_of_typed_infrared_ast (statements : TypedInfraredAst.statement list) (env : environment) : string =
  let statement_strings = List.map (fun s -> string_of_typed_infrared_statement s env) statements in
  let joined_statement_strings = String.concat "\n" statement_strings in
  "\n" ^ joined_statement_strings

let string_of_ast (ast : Loc.t FlowAst.program * (Loc.t * Err.t) list) : string =
  let flow_ast = FlowPrinter.string_of_ast ast in
  flow_ast

let string_of_program (prog: program) : string =
  match prog with
  | FlowProgram (ast, errs) -> FlowPrinter.string_of_ast (ast, errs)
  | InfraredProgram (statements) -> string_of_infrared_ast statements
  | TypedInfraredProgram (statements, env) -> string_of_typed_infrared_ast statements env

let string_of_title (title : string) : string =
  let ending_str = "<><><><><><><><><><><><><><><><><><><><><><><><><><>" in
  let length_of_ending_str = String.length ending_str in
  let length_of_title = String.length title in
  let final_ending_str =
    Printf.sprintf "%s ✨"
      (String.sub ending_str 0
         (Utils.math_max
            (length_of_ending_str - length_of_title + 8) 0))
  in
  Printf.sprintf "%s %s %s"
    (Chalk.cyan "<><>")
    (Chalk.bold title)
    (Chalk.cyan final_ending_str)

let pprint_program_with_title (title : string) (program : program) : program =
  let title = string_of_title title in
  Printf.printf "%s %s\n\n"
    title
    (string_of_program program);
  program
