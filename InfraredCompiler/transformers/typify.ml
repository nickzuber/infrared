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
  let (loc, statement) = statement in
  match statement with
  | VariableDeclaration ((id_loc, id), expression) ->
    let d_type = type_of_expression expression env in
    let typed_expression = (d_type, expression) in
    Hashtbl.replace env id d_type;
    (loc, VariableDeclaration ((id_loc, id), typed_expression))
  | FunctionDeclaration ((name_loc, name), params, body) ->
    let typed_body = List.map (fun s -> typify_statement s env) body in
    (loc, FunctionDeclaration ((name_loc, name), params, typed_body))
  | Return expr ->
    let d_type = type_of_expression expr env in
    let typed_expression = (d_type, expr) in
    (loc, Return typed_expression)
  | Expression expr ->
    let d_type = type_of_expression expr env in
    let typed_expression = (d_type, expr) in
    (loc, Expression typed_expression)
  | Block statements ->
    let statements' = List.map (fun s -> typify_statement s env) statements in
    (loc, Block statements')
  | If (expr, s1, s2) ->
    let d_type = type_of_expression expr env in
    let typed_expression = (d_type, expr) in
    let s1' = typify_statement s1 env in
    let s2' = typify_statement s2 env in
    (loc, If (typed_expression, s1', s2'))

and type_of_expression (expression : expression) (env : environment) : data_type =
  let (_, expression) = expression in
  match expression with
  | String _ -> Primative String
  | Number _ -> Primative Number
  | Boolean _ -> Primative Boolean
  | Null -> Primative Null
  | Undefined -> Primative Undefined
  | Variable (_, id) -> get_type env id
  | BinaryOperation (_op, left, right) ->
    let d_type_left = type_of_expression left env in
    let d_type_right = type_of_expression right env in
    Reduction [d_type_left; d_type_right]
  | Object pairs ->
    let typed_pairs = List.map (fun pair ->
        let ((_, key), value) = pair in
        let d_type = type_of_expression value env in
        (key, d_type)
      ) pairs in
    Primative (Object typed_pairs)
  | Access (e1, e2) ->
    let inner_d_type = type_of_expression e1 env in
    Drill (inner_d_type, e2)
  | Call (callee, args) ->
    let callee_d_type = type_of_expression callee env in
    let args_d_types = List.map (fun arg -> type_of_expression arg env) args in
    Exec (callee_d_type, args_d_types)
  | _ -> Generic "typify-todo-expression"

let transform (env : environment) (program : program) : program =
  match program with
  | InfraredProgram (statements) ->
    let statements' = List.map (fun statement -> typify_statement statement env) statements in
    TypedInfraredProgram (statements', env)
  | _ -> raise (Unexpected_compiler_phase "Expected InfraredProgram")
