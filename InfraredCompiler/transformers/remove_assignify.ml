open InfraredParser.Ast
open InfraredAst

exception Unexpected_compiler_phase of string

let rec remove_assignify_statement (statement : statement) : statement =
  let (loc, statement) = statement in
  match statement with
  | Expression (expression_loc, Assignment (id, expr)) ->
    (* Give this statement the expression's location for now because I think when
     * it comes time to show the message, this location will be more accurate. *)
    (expression_loc, VariableDeclaration (id, expr))
  | FunctionDeclaration (name, args, body) ->
    let body' = List.map remove_assignify_statement body in
    (loc, FunctionDeclaration (name, args, body'))
  | _ -> (loc, statement)

let transform (program : program) : program =
  match program with
  | InfraredProgram (statements) ->
    let statements' = List.map (fun statement -> remove_assignify_statement statement) statements in
    InfraredProgram statements'
  | _ -> raise (Unexpected_compiler_phase "Expected InfraredProgram")
