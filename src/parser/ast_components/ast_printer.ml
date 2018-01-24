
(* Raw json, format using another tool *)

let remove_trailing_comma str =
  let len = String.length str in
  if len > 0 && str.[len - 1] = ',' then
    String.sub str 0 (len - 1)
  else
    str


(* Independents *)
let pp_literal_numeric_expression node =
  let open Ast.LiteralNumericExpression in
  let json = Printf.sprintf
    "{\"LiteralNumericExpression\": { \
      \"type\": \"LiteralNumericExpression\", \
      \"value\": %f \
    }},"
    node.value
  in json

let pp_binary_operator node =
  let open Ast.BinaryOperator in
  match node with
  | Equal -> "\"Equal\""
  | NotEqual -> "\"NotEqual\""
  | StrictEqual -> "\"StrictEqual\""
  | StrictNotEqual -> "\"StrictNotEqual\""
  | LessThan -> "\"LessThan\""
  | LessThanEqual -> "\"LessThanEqual\""
  | GreaterThan -> "\"GreaterThan\""
  | GreaterThanEqual -> "\"GreaterThanEqual\""
  | In -> "\"In\""
  | Instanceof -> "\"Instanceof\""
  | LeftShift -> "\"LeftShift\""
  | RightShift -> "\"RightShift\""
  | RightShiftUnsigned -> "\"RightShiftUnsigned\""
  | Plus -> "\"Plus\""
  | Minus -> "\"Minus\""
  | Mult -> "\"Mult\""
  | Div -> "\"Div\""
  | Mod -> "\"Mod\""
  | Pow -> "\"Pow\""
  | Comma -> "\"Comma\""
  | LogicalOr -> "\"LogicalOr\""
  | LogicalAnd -> "\"LogicalAnd\""
  | Or -> "\"Or\""
  | Xor -> "\"Xor\""
  | And -> "\"And\""

(* Dependents *)
let rec pp_expression node =
  let open Ast.Expression in
  let json = match node with
  | BinaryExpression node -> pp_binary_expression node
  | LiteralNumericExpression node -> pp_literal_numeric_expression node
  | LiteralBooleanExpression node -> pp_literal_boolean_expression node
  | IdentifierExpression node -> pp_identifier_expression node
  | CallExpression node -> pp_call_expression node
  | AssignmentExpression node -> pp_assignment_expression node
  | _ -> "\"Unimplemented Expression type\""
  in remove_trailing_comma json

and pp_literal_boolean_expression node =
  let open Ast.LiteralBooleanExpression in
  let json = Printf.sprintf
    "{\"LiteralBooleanExpression\": { \
      \"type\": \"LiteralBooleanExpression\", \
      \"value\": %s \
    }},"
    (string_of_bool node.value)
  in json

and pp_assignment_expression node =
  let open Ast.AssignmentExpression in
  let binding = pp_assignment_target node.binding in
  let expression = pp_expression node.expression in
  let json = Printf.sprintf
    "{\"AssignmentExpression\": { \
      \"type\": \"AssignmentExpression\", \
      \"binding\": %s, \
      \"expression\": %s \
    }},"
    binding
    expression
  in json

and pp_assignment_target node =
  let open Ast.AssignmentTarget in
  let json = match node with
    | AssignmentTargetPattern node -> pp_assignment_target_pattern node
    | SimpleAssignmentTarget node -> pp_simple_assignment_target node
  in remove_trailing_comma json

and pp_assignment_target_pattern node =
  let open Ast.AssignmentTargetPattern in
  let json = match node with
    | ObjectAssignmentTarget node -> "\"Unimplemented ObjectAssignmentTarget\""
    | ArrayAssignmentTarget node -> "\"Unimplemented ArrayAssignmentTarget\""
  in remove_trailing_comma json

and pp_simple_assignment_target node =
  let open Ast.SimpleAssignmentTarget in
  let json = match node with
    | AssignmentTargetIdentifier node -> pp_assignment_target_identifier node
    | MemberAssignmentTarget node -> "\"Unimplemented MemberAssignmentTarget\""
  in remove_trailing_comma json

and pp_assignment_target_identifier node =
  let open Ast.AssignmentTargetIdentifier in
  let json = Printf.sprintf
    "{\"AssignmentTargetIdentifier\": { \
      \"type\": \"AssignmentTargetIdentifier\", \
      \"name\": \"%s\" \
    }},"
    node.name
  in json

and pp_binary_expression node =
  let open Ast.BinaryExpression in
  let operator = pp_binary_operator node.operator in
  let left = pp_expression node.left in
  let right = pp_expression node.right in
  let json = Printf.sprintf
    "{\"BinaryExpression\": { \
      \"type\": \"BinaryExpression\", \
      \"left\": %s, \
      \"operator\": %s, \
      \"right\": %s \
    }},"
    left
    operator
    right
  in json

and pp_call_expression node =
  let open Ast.CallExpression in
  let callee = match node.callee with
    | Expression node -> pp_expression node
    | Super node -> "\"Unimplemented CallExpression.callee.super type\"" in
  let arguments =
    List.fold_left
      (fun acc item ->
        let item = pp_argument item
        in acc ^ item ^ ",")
      "" node.arguments in
  let json = Printf.sprintf
    "{\"CallExpression\": { \
      \"type\": \"CallExpression\", \
      \"callee\": %s, \
      \"arguments\": [%s] \
    }},"
    callee
    (remove_trailing_comma arguments)
  in json

and pp_argument node =
  let open Ast.Arguments in
  let json = match node with
    | SpreadElement node -> pp_spread_element node
    | Expression node -> pp_expression node
  in remove_trailing_comma json

and pp_spread_element node =
  let open Ast.SpreadElement in
  let expression = pp_expression node.expression in
  let json = Printf.sprintf
    "{\"SpreadElement\": { \
      \"type\": \"SpreadElement\", \
      \"expression\": %s \
    }},"
    expression
  in json

and pp_identifier_expression node =
  let open Ast.IdentifierExpression in
  let json = Printf.sprintf
    "{\"IdentifierExpression\": { \
      \"type\": \"IdentifierExpression\", \
      \"name\": \"%s\" \
    }},"
    node.name
  in json

and pp_binding_identifier node =
  let open Ast.BindingIdentifier in
  let json = Printf.sprintf
    "{\"BindingIdentifier\": { \
      \"type\": \"BindingIdentifier\", \
      \"name\": \"%s\" \
    }},"
    node.name
  in json

and pp_variable_declarator node =
  let open Ast.VariableDeclarator in
  let binding = pp_binding_identifier node.binding in
  let init = match node.init with
    | Some node -> pp_expression node
    | None -> "null" in
  let json = Printf.sprintf
    "{\"VariableDeclarator\": { \
      \"type\": \"VariableDeclarator\", \
      \"binding\": %s, \
      \"init\": [%s] \
    }},"
    (remove_trailing_comma binding)
    init
  in json

and pp_variable_declaration node =
  let open Ast.VariableDeclaration in
  let kind = let open Ast.VariableDeclarationKind in
    match node.kind with
    | Var -> "\"Var\""
    | Let -> "\"Let\""
    | Const -> "\"Const\"" in
  let declarators =
    List.fold_left
      (fun acc declarator -> acc ^ (pp_variable_declarator declarator))
      "" node.declarators in
  let json = Printf.sprintf
    "{\"VariableDeclaration\": { \
      \"type\": \"VariableDeclaration\", \
      \"kind\": %s, \
      \"declarators\": [%s] \
    }},"
    kind
    (remove_trailing_comma declarators)
  in json

and pp_variable_declaration_statement node =
  let open Ast.VariableDeclarationStatement in
  let declaration = pp_variable_declaration node.declaration in
  let json = Printf.sprintf
    "{\"VariableDeclarationStatement\": { \
      \"type\": \"VariableDeclarationStatement\", \
      \"declaration\": %s \
    }},"
    (remove_trailing_comma declaration)
  in json

and pp_statement node =
  let open Ast.Statement in
  let json = match node with
    | VariableDeclarationStatement node -> pp_variable_declaration_statement node
    | ExpressionStatement node -> pp_expression_statement node
    | _ -> "\"Unimplemented Statement type\""
  in remove_trailing_comma json

and pp_expression_statement node =
  let open Ast.ExpressionStatement in
  let expression = pp_expression node.expression in
  let json = Printf.sprintf
    "{\"ExpressionStatement\": { \
      \"type\": \"ExpressionStatement\", \
      \"expression\": %s \
    }},"
    (remove_trailing_comma expression)
  in json

and pp_directive node =
  let open Ast.Directive in
  let json = Printf.sprintf
    "{\"Directive\": { \
      \"type\": \"Directive\", \
      \"rawValue\": \"%s\" \
    }},"
    node.rawValue
  in json

and pp_module node =
  let open Ast.Module in
  let directives =
    List.fold_left
      (fun acc directive -> acc ^ (pp_directive directive))
      "" node.directives in
  let items =
    List.fold_left
      (fun acc item ->
        let item = match item with
        | Statement node -> pp_statement node
        | _ -> "\"Unimplemented Module.items type\""
        in acc ^ item ^ ",")
      "" node.items in
  let json = Printf.sprintf
    "{\"Module\": { \
      \"type\": \"Module\", \
      \"directives\": [%s], \
      \"items\": [%s] \
    }}"
    (remove_trailing_comma directives)
    (remove_trailing_comma items)
  in json

and pp_program node =
  let open Ast.Program in
  let json = match node with
  | Module node -> pp_module node
  | _ -> "\"Unimplemented Program type\""
  in remove_trailing_comma json
