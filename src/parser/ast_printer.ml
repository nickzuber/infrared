
(* Raw json, format using another tool *)

let pp_variable_declaration node =
  let open Ast.VariableDeclaration in
  let kind = let open Ast.VariableDeclarationKind in
    match node.kind with
    | Var -> "\"Var\""
    | Let -> "\"Let\""
    | Const -> "\"Const\"" in
  let declarators = "\"declarators\""
  in Printf.sprintf
    "{\"VariableDeclaration\": { \
      \"kind\": %s, \
      \"declarators\": [%s], \
    }},"
    kind
    declarators


let pp_variable_declaration_statement node =
  let open Ast.VariableDeclarationStatement in
  let declaration = pp_variable_declaration node.declaration
  in Printf.sprintf
    "{\"VariableDeclarationStatement\": { \
      \"declaration\": %s \
    }},"
    declaration


let pp_statement node =
  let open Ast.Statement in
  match node with
  | VariableDeclarationStatement node -> pp_variable_declaration_statement node
  | _ -> "\"Unimplemented Statement type\""

let pp_directive node =
  let open Ast.Directive in
  Printf.sprintf
    "{\"Directive\": { \
      \"rawValue\": \"%s\" \
    }},"
    node.rawValue

let pp_module node =
  let open Ast.Module in
  let directives =
    (List.fold_left
      (fun acc directive -> acc ^ (pp_directive directive))
      "" node.directives) in
  let items = 
      (List.fold_left
        (fun acc item -> 
          match item with
          | Statement node -> pp_statement node
          | _ -> "\"Unimplemented Module.items type\"")
        "" node.items)
  in Printf.sprintf
    "{\"Module\": { \
      \"directives\": [%s], \
      \"items\": [%s], \
    }}"
    directives
    items

let pp_program node =
  let open Ast.Program in
  match node with
  | Module node -> pp_module node
  | _ -> "\"Unimplemented Program type\""
