open InfraredParser.Ast
open TypedInfraredAst

exception Unexpected_compiler_phase of string
exception Unexpected_function_data_type of string

let get_type tbl key =
  try Hashtbl.find tbl key
  with _ -> Generic "unable-to-find-this-type-this-is-a-bug"

let rec flatten_statement (statement : statement) : statement list =
  let (loc, statement) = statement in
  match statement with
  | Block statements -> flatten_statements statements
  | If (_, s1, s2) -> List.append (flatten_statement s1) (flatten_statement s2)
  | _ -> [(loc, statement)]

and flatten_statements (statements : statement list) : statement list =
  let flat_statements = List.map flatten_statement statements in
  List.flatten flat_statements

let get_all_return_d_types (statements : statement list) : data_type list =
  let statements' = flatten_statements statements in
  List.filter_map (fun statement ->
      let (_, statement) = statement in
      match statement with
      | Return (d_type, _) -> Some d_type
      | _ -> None
    ) statements'

let get_param_d_types (d_type : data_type) : data_type list =
  match d_type with
  | Primative (Function (param_d_types, _)) -> param_d_types
  | _ ->
    raise
      (Unexpected_function_data_type "A function was found with a non-function data type")

let rec function_returns_refine_statement (statement : statement) (env : environment) : statement =
  let (loc, statement) = statement in
  match statement with
  | FunctionDeclaration ((name_loc, name), params, body) ->
    let prev_d_type = get_type env name in
    let param_d_types = get_param_d_types prev_d_type in
    let return_d_type = get_return_type_of_function body in
    let d_type = Primative (Function (param_d_types, return_d_type)) in
    Hashtbl.replace env name d_type;
    let body' = List.map (fun s -> function_returns_refine_statement s env) body in
    (loc, FunctionDeclaration ((name_loc, name), params, body'))
  | _ -> (loc, statement)

and get_return_type_of_function (statements : statement list) : data_type =
  let return_statement_d_types = get_all_return_d_types statements in
  match List.length return_statement_d_types with
  | 0 -> Primative Undefined
  | 1 -> List.hd return_statement_d_types
  | _ -> Union return_statement_d_types

(* The environment is just mutated here and it doesn't actually conver this into a
 * typed program. This is important because this phase happens before we begin to
 * typify the program. Function declarations are statements, so they don't actually
 * have a "type" like an expression would, but rather defines an identifier with a
 * type in the environment. *)
let transform  (env : environment) (program : program) : program =
  match program with
  | TypedInfraredProgram (statements, _env) ->
    let statements' = List.map
        (fun statement -> function_returns_refine_statement statement env)
        statements
    in
    TypedInfraredProgram (statements', env)
  | _ -> raise (Unexpected_compiler_phase "Expected TypedInfraredProgram")
