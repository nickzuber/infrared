(* open InfraredUtils *)
open InfraredParser.Ast
open InfraredAst

exception Unexpected_compiler_phase of string

let typify_statement (statement : statement) (_env : environment) : data_type =
  match statement with
  | _ -> Primative Undefined

let typify_expression (expression : expression) (_env : environment) : data_type =
  match expression with
  | _ -> Primative Undefined

let transform (program : program) : program =
  let env : environment = Hashtbl.create 53 in
  match program with
  | InfraredProgram (statements) ->
    let _ = List.map (fun statement -> typify_statement statement env) statements
    in
    TypedInfraredProgram (statements, env)
  | _ -> raise (Unexpected_compiler_phase "Expected InfraredProgram")
