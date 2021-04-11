open InfraredParser.Ast
open InfraredAst

exception Unexpected_compiler_phase of string

let rec remove_assignify_statement (statement : statement) : statement =
  match statement with
  | Expression (Assignment (id, expr)) ->
    VariableDeclaration (id, expr)
  | FunctionDeclaration (name, args, body) ->
    let body' = List.map remove_assignify_statement body in
    FunctionDeclaration (name, args, body')
  | _ -> statement

let transform (program : program) : program =
  match program with
  | InfraredProgram (statements) ->
    let statements' = List.map (fun statement -> remove_assignify_statement statement) statements in
    InfraredProgram statements'
  | _ -> raise (Unexpected_compiler_phase "Expected InfraredProgram")
