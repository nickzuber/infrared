open InfraredParser.Ast
open InfraredAst

exception Unexpected_compiler_phase of string

let hoist_statement (statement : statement) (env : environment) : statement =
  match statement with
  | FunctionDeclaration (name, params, body) ->
    let param_d_types = List.map (fun id ->
        let generic = Generic id in
        Hashtbl.replace env id generic;
        generic
      ) params
    in
    let return_d_type = Primative Undefined in
    let d_type = Primative (Function (param_d_types, return_d_type)) in
    Hashtbl.replace env name d_type;
    FunctionDeclaration (name, params, body)
  | _ -> statement

(* The environment is just mutated here and it doesn't actually conver this into a
 * typed program. This is important because this phase happens before we begin to
 * typify the program. Function declarations are statements, so they don't actually
 * have a "type" like an expression would, but rather defines an identifier with a
 * type in the environment. *)
let transform  (env : environment) (program : program) : program =
  match program with
  | InfraredProgram (statements) ->
    let statements' = List.map (fun statement -> hoist_statement statement env) statements in
    InfraredProgram statements'
  | _ -> raise (Unexpected_compiler_phase "Expected InfraredProgram")
