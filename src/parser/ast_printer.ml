
(* Raw json, format using another tool *)

let remove_trailing_comma str =
  let len = String.length str in
  if len > 0 && str.[len - 1] = ',' then
    String.sub str 0 (len - 1)
  else
    str

let pp_literal_numeric_expression node =
  let open Ast.LiteralNumericExpression in
  let json = Printf.sprintf
    "{\"LiteralNumericExpression\": { \
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

let rec pp_expression node =
  let open Ast.Expression in
  let json = match node with
  | BinaryExpression node -> pp_binary_expression node
  | LiteralNumericExpression node -> pp_literal_numeric_expression node
  | IdentifierExpression node -> pp_identifier_expression node
  | _ -> "\"Unimplemented Expression type\""
  in remove_trailing_comma json

and pp_binary_expression node =
  let open Ast.BinaryExpression in
  let operator = pp_binary_operator node.operator in
  let left = pp_expression node.left in
  let right = pp_expression node.right in
  let json = Printf.sprintf
    "{\"BinaryExpression\": { \
      \"left\": %s, \
      \"operator\": %s, \
      \"right\": %s \
    }},"
    left
    operator
    right
  in json

and pp_identifier_expression node =
  let open Ast.IdentifierExpression in
  let json = Printf.sprintf
    "{\"IdentifierExpression\": { \
      \"name\": \"%s\" \
    }},"
    node.name
  in json

let pp_binding_identifier node =
  let open Ast.BindingIdentifier in
  let json = Printf.sprintf
    "{\"VariableDeclarator\": { \
      \"name\": \"%s\" \
    }},"
    node.name
  in json

let pp_variable_declarator node =
  let open Ast.VariableDeclarator in
  let binding = pp_binding_identifier node.binding in
  let init = match node.init with
    | Some node -> pp_expression node
    | None -> "null" in
  let json = Printf.sprintf
    "{\"VariableDeclarator\": { \
      \"binding\": %s, \
      \"init\": [%s] \
    }},"
    (remove_trailing_comma binding)
    init
  in json

let pp_variable_declaration node =
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
      \"kind\": %s, \
      \"declarators\": [%s] \
    }},"
    kind
    (remove_trailing_comma declarators)
  in json

let pp_variable_declaration_statement node =
  let open Ast.VariableDeclarationStatement in
  let declaration = pp_variable_declaration node.declaration in
  let json = Printf.sprintf
    "{\"VariableDeclarationStatement\": { \
      \"declaration\": %s \
    }},"
    (remove_trailing_comma declaration)
  in json

let pp_statement node =
  let open Ast.Statement in
  let json = match node with
  | VariableDeclarationStatement node -> pp_variable_declaration_statement node
  | _ -> "\"Unimplemented Statement type\""
  in remove_trailing_comma json

let pp_directive node =
  let open Ast.Directive in
  let json = Printf.sprintf
    "{\"Directive\": { \
      \"rawValue\": \"%s\" \
    }},"
    node.rawValue
  in json

let pp_module node =  
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
      \"directives\": [%s], \
      \"items\": [%s] \
    }}"
    (remove_trailing_comma directives)
    (remove_trailing_comma items)
  in json

let pp_program node =
  let open Ast.Program in
  let json = match node with
  | Module node -> pp_module node
  | _ -> "\"Unimplemented Program type\""
  in remove_trailing_comma json
