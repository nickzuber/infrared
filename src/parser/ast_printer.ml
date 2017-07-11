
(* Raw json, format using another tool *)

let remove_trailing_comma str =
  let len = String.length str in
  if len > 0 && str.[len - 1] = ',' then
    String.sub str 0 (len - 1)
  else
    str

let pp_expression node =
  "\"(expression)\""

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
  let init = pp_expression node.init in
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
        match item with
        | Statement node -> pp_statement node
        | _ -> "\"Unimplemented Module.items type\"")
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
