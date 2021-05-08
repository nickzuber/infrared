open Ast
open InfraredUtils
module FlowAst = Flow_parser.Ast
module Loc = Flow_parser.Loc
module Err = Flow_parser.Parse_error

let string_of_type (t : string) : string =
  Printf.sprintf "[%s] "
    (Chalk.green t)

let get_type tbl key =
  try Hashtbl.find tbl key
  with _ -> Generic "unable-to-find-this-type-this-is-a-bug"

let rec string_of_infrared_statement ?depth:(depth=0) ?tag:(tag="") (statement: InfraredAst.statement) : string =
  let open InfraredAst in
  let string_of_type (t : string) = string_of_type (tag ^ t) in
  let padding = String.make depth '\t' in
  let (_, statement) = statement in
  match statement with
  | VariableDeclaration ((_, id), value) ->
    Printf.sprintf "%s%s %s <- %s"
      padding
      (string_of_type "VariableDeclaration")
      id
      (string_of_infrared_expression ~depth:depth value)
  | FunctionDeclaration ((_, name), params, body) ->
    let params_no_loc = List.map (fun (_, p) -> p) params in
    let params' = String.concat ", " params_no_loc in
    let body' = String.concat "\n"
        (List.map (string_of_infrared_statement ~depth:(depth + 1))
           body)
    in
    Printf.sprintf "%s%s %s (%s) {\n%s\n%s}"
      padding
      (string_of_type "FunctionDeclaration")
      name
      (params')
      (body')
      padding
  | Return expr ->
    Printf.sprintf "%s%sreturn %s"
      padding
      (string_of_type "Return")
      (string_of_infrared_expression ~depth:depth expr)
  | Expression expr ->
    Printf.sprintf "%s%s%s"
      padding
      (string_of_type "Expression")
      (string_of_infrared_expression ~depth:depth expr)
  | If (expr, s1, s2) ->
    Printf.sprintf "%s%sif (%s)\n%s\n%s"
      padding
      (string_of_type "IfStatement")
      (string_of_infrared_expression ~depth:(depth + 1) expr)
      (string_of_infrared_statement ~depth:(depth + 1) ~tag:("Then") s1)
      (string_of_infrared_statement ~depth:(depth + 1) ~tag:("Else") s2)
  | Block statements ->
    let statement_strs = List.map (string_of_infrared_statement ~depth:(depth + 1)) statements in
    Printf.sprintf "%s%s{\n%s\n%s}"
      padding
      (string_of_type "Block")
      (String.concat "\n" statement_strs)
      padding

and string_of_infrared_expression ?depth:(depth=0) (expression : InfraredAst.expression) : string =
  let open InfraredAst in
  let padding = String.make depth '\t' in
  let (_, expression) = expression in
  match expression with
  | Variable (_, id) -> id
  | String s -> "\"" ^ s ^ "\""
  | Number n -> string_of_int n
  | Boolean b -> string_of_bool b
  | Null -> "null"
  | Undefined -> "undefined"
  | BinaryOperation (binop, left, right) ->
    Printf.sprintf "%s %s %s"
      (string_of_infrared_expression ~depth:depth left)
      (string_of_infrared_binop binop)
      (string_of_infrared_expression ~depth:depth right)
  | Assignment ((_, id), expr) ->
    Printf.sprintf "%s = %s"
      (id)
      (string_of_infrared_expression ~depth:depth expr)
  | Object pairs ->
    let pairs_strs = List.map (fun pair ->
        let ((_, key), value) = pair in
        let value_str = string_of_infrared_expression ~depth:(depth + 1) value in
        Printf.sprintf "\n\t%s%s: %s"
          padding
          key
          value_str
      ) pairs
    in
    let formatted_pairs = String.concat "," pairs_strs in
    Printf.sprintf "{%s\n%s}"
      formatted_pairs
      padding
  | Access (e1, e2) ->
    Printf.sprintf "(%s%s)"
      (string_of_infrared_expression ~depth:depth e1)
      (string_of_infrared_property e2)
  | Call (callee, args) ->
    let pp_args = List.map (string_of_infrared_expression ~depth:depth) args in
    let pp_args_str = String.concat ", " pp_args in
    Printf.sprintf "%s(%s)"
      (string_of_infrared_expression ~depth:depth callee)
      pp_args_str
  | _ -> string_of_type "#<unhandled_expression>"

and string_of_infrared_property (prop : InfraredAst.property) : string =
  let open InfraredAst in
  match prop with
  | PropertyExpression expr -> "[" ^ (string_of_infrared_expression expr) ^ "]"
  | PropertyIdentifier (_, id) -> "." ^ id

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

let rec string_of_primative ?depth:(depth=0) (prim : primative_data_type) : string =
  let padding = String.make depth '\t' in
  match prim with
  | Null -> "Null"
  | Undefined -> "Undefined"
  | String -> "String"
  | Boolean -> "Boolean"
  | Number -> "Number"
  | Object pairs ->
    let pp_pairs = List.map (fun pair ->
        let (key, d_type) = pair in
        Printf.sprintf "\n%s\t%s: %s"
          padding
          key
          (string_of_data_type ~depth:(depth + 1) d_type)
      ) pairs
    in
    Printf.sprintf "Object {%s\n%s}"
      (String.concat "; " pp_pairs)
      padding
  | Array _ -> "Array"
  | Function (params, ret) ->
    let pp_params = List.map (string_of_data_type ~depth:depth) params in
    let pp_params_str = String.concat " → " pp_params in
    let pp_ret = string_of_data_type ~depth:depth ret in
    Printf.sprintf "(%s) → %s"
      pp_params_str
      pp_ret

and string_of_data_type ?depth:(depth=0) (d_type : data_type) : string =
  match d_type with
  | Generic tag -> "'" ^ tag
  | Defer _expr -> "Defer"
  | Exec (callee_d_type, args_d_types) ->
    let pp_args_d_types = List.map (string_of_data_type ~depth:depth) args_d_types in
    let pp_args_d_types_str = String.concat ", " pp_args_d_types in
    Printf.sprintf "App(%s, (%s))"
      (string_of_data_type ~depth:depth callee_d_type)
      (pp_args_d_types_str)
  | Primative prim -> string_of_primative ~depth:depth prim
  | Reduction d_types ->
    let d_types_str = List.map (string_of_data_type ~depth:depth) d_types in
    let str = String.concat ", " d_types_str in
    Printf.sprintf "Reduce(%s)"
      str
  | Union d_types ->
    let d_types_str = List.map (string_of_data_type ~depth:depth) d_types in
    let str = String.concat " ∨ " d_types_str in
    Printf.sprintf "Union(%s)"
      str
  | Drill (d_type, prop) ->
    Printf.sprintf "Access(%s, %s)"
      (string_of_data_type ~depth:depth d_type)
      (string_of_infrared_property prop)
  | Unknown -> Chalk.red "Unknown"

let pp_string_of_data_type ?depth:(depth=0) (d_type : data_type) : string =
  let d_type_str = string_of_data_type ~depth:depth d_type in
  let str = "⊢ " ^ d_type_str in
  str |> Chalk.white |> Chalk.bold

let rec string_of_typed_infrared_statement ?depth:(depth=0) ?tag:(tag="") (statement: TypedInfraredAst.statement) (env : environment) : string =
  let open TypedInfraredAst in
  let string_of_type (t : string) = string_of_type (tag ^ t) in
  let padding = String.make depth '\t' in
  let (_, statement) = statement in
  match statement with
  | VariableDeclaration ((_, id), typed_value) ->
    let (d_type, value) = typed_value in
    Printf.sprintf "%s%s %s <- %s %s"
      padding
      (string_of_type "VariableDeclaration")
      id
      (string_of_infrared_expression ~depth:depth value)
      (pp_string_of_data_type ~depth:depth d_type)
  | FunctionDeclaration ((_, name), params, body) ->
    let typed_params = List.map (fun (_, param) ->
        let d_type = get_type env param in
        (d_type, param)
      ) params
    in
    let formatted_typed_params = List.map (fun typed_param ->
        let (d_type, param) = typed_param in
        Printf.sprintf "%s %s"
          param
          (pp_string_of_data_type ~depth:depth d_type)
      ) typed_params
    in
    let params' = String.concat ", " formatted_typed_params in
    let body' = String.concat "\n"
        (List.map (fun s -> string_of_typed_infrared_statement ~depth:(depth + 1) s env)
           body)
    in
    let return_d_type = get_type env name in
    Printf.sprintf "%s%s %s (%s) %s {\n%s\n%s}"
      padding
      (string_of_type "FunctionDeclaration")
      name
      (params')
      (pp_string_of_data_type ~depth:depth return_d_type)
      (body')
      padding
  | Return expr ->
    let (d_type, expr) = expr in
    Printf.sprintf "%s%sreturn %s %s"
      padding
      (string_of_type "Return")
      (string_of_infrared_expression ~depth:depth expr)
      (pp_string_of_data_type ~depth:depth d_type)
  | Expression typed_expr ->
    let (d_type, expr) = typed_expr in
    Printf.sprintf "%s%s %s %s"
      padding
      (string_of_type "Expression")
      (string_of_infrared_expression ~depth:depth expr)
      (pp_string_of_data_type ~depth:depth d_type)
  | If (expr, s1, s2) ->
    let (d_type, expr) = expr in
    Printf.sprintf "%s%sif (%s %s)\n%s\n%s"
      padding
      (string_of_type "IfStatement")
      (string_of_infrared_expression ~depth:(depth + 1) expr)
      (pp_string_of_data_type ~depth:depth d_type)
      (string_of_typed_infrared_statement ~depth:(depth + 1) ~tag:("Then") s1 env)
      (string_of_typed_infrared_statement ~depth:(depth + 1) ~tag:("Else") s2 env)
  | Block statements ->
    let statement_strs = List.map
        (fun s -> string_of_typed_infrared_statement ~depth:(depth + 1) s env) statements in
    Printf.sprintf "%s%s{\n%s\n%s}"
      padding
      (string_of_type "Block")
      (String.concat "\n" statement_strs)
      padding

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
  let ending_str = "<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>" in
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
