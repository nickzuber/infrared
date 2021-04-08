(* open InfraredUtils *)
open InfraredParser.Ast
open InfraredAst

exception Unexpected_compiler_phase of string

let get_type tbl key =
  try Hashtbl.find tbl key
  with _ -> Primative Undefined

let get_type_of_argument tbl key =
  try Hashtbl.find tbl key
  with _ -> Generic key

let rec typify_statement (statement : statement) (env : environment) : TypedInfraredAst.statement =
  match statement with
  | VariableDeclaration (id, expression) ->
    let d_type = type_of_expression expression env in
    let typed_expression = (d_type, expression) in
    Hashtbl.replace env id d_type;
    VariableDeclaration (id, typed_expression)
  | FunctionDeclaration (name, params, body) ->
    let _ = List.map (fun id ->
        let generic = Generic id in
        Hashtbl.replace env id generic;
      ) params
    in
    let typed_body = List.map (fun s -> typify_statement s env) body in
    FunctionDeclaration (name, params, typed_body)
  | Return expr ->
    let d_type = type_of_expression expr env in
    let typed_expression = (d_type, expr) in
    Return typed_expression
  | Expression expr ->
    let d_type = type_of_expression expr env in
    let typed_expression = (d_type, expr) in
    Expression typed_expression
  | _ -> TypedInfraredAst.Expression
           (Generic "todo-statement", Undefined)

and type_of_expression (expression : expression) (env : environment) : data_type =
  match expression with
  | String _ -> Primative String
  | Number _ -> Primative Number
  | Boolean _ -> Primative Boolean
  | Null -> Primative Null
  | Undefined -> Primative Undefined
  | Variable id -> get_type env id
  | BinaryOperation (_op, left, right) ->
    let d_type_left = type_of_expression left env in
    let d_type_right = type_of_expression right env in
    Reduction [d_type_left; d_type_right]
  | _ -> Generic "todo-expression"

let transform (program : program) : program =
  let env : environment = Hashtbl.create 53 in
  match program with
  | InfraredProgram (statements) ->
    let statements' = List.map (fun statement -> typify_statement statement env) statements in
    TypedInfraredProgram (statements', env)
  | _ -> raise (Unexpected_compiler_phase "Expected InfraredProgram")
