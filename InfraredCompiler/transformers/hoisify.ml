open InfraredParser.Ast
open InfraredAst

exception Unexpected_compiler_phase of string

let get_all_return_expressions (statements : statement list) : statement list =
  List.filter (fun statement ->
      match statement with
      | Return _ -> true
      | _ -> false)
    statements

let rec hoist_statement (statement : statement) (env : environment) : statement =
  match statement with
  | FunctionDeclaration (name, params, body) ->
    let param_d_types = List.map (fun id ->
        let generic = Generic id in
        (* Assign types to arguments *)
        Hashtbl.replace env id generic;
        generic
      ) params
    in
    let return_d_type = get_return_type_of_function body in
    (* Assign types to inner body function declarations *)
    let _ = List.map (fun statement -> hoist_statement statement env) body in
    let d_type = Primative (Function (param_d_types, return_d_type)) in
    Hashtbl.replace env name d_type;
    FunctionDeclaration (name, params, body)
  | _ -> statement

and get_return_type_of_function (statements : statement list) : data_type =
  let return_statements = get_all_return_expressions statements in
  match List.length return_statements with
  | 0 -> Primative Undefined
  | _ as n -> Generic ((string_of_int n) ^ "todo-get-function-return-type")

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
