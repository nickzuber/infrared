open InfraredParser.Ast
open InfraredAst

exception Unexpected_compiler_phase of string

let rec init_func_statement (statement : statement) (env : environment) : statement =
  let (loc, statement) = statement in
  match statement with
  | FunctionDeclaration ((name_loc, name), params, body) ->
    let param_d_types = List.map (fun (_, id) ->
        let generic = Generic id in
        (* Assign types to arguments *)
        Hashtbl.replace env id generic;
        generic
      ) params
    in
    let return_d_type = Unknown in
    (* Assign types to inner body function declarations *)
    let _ = List.map (fun statement -> init_func_statement statement env) body in
    let d_type = Primative (Function (param_d_types, return_d_type)) in
    Hashtbl.replace env name d_type;
    (loc, FunctionDeclaration ((name_loc, name), params, body))
  | _ -> (loc, statement)

(* The environment is just mutated here and it doesn't actually conver this into a
 * typed program. This is important because this phase happens before we begin to
 * typify the program. Function declarations are statements, so they don't actually
 * have a "type" like an expression would, but rather defines an identifier with a
 * type in the environment. *)
let transform  (env : environment) (program : program) : program =
  match program with
  | InfraredProgram (statements) ->
    let statements' = List.map (fun statement -> init_func_statement statement env) statements in
    InfraredProgram statements'
  | _ -> raise (Unexpected_compiler_phase "Expected InfraredProgram")
