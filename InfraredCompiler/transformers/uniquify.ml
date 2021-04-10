open InfraredUtils
open InfraredParser.Ast
open InfraredAst

exception Unexpected_compiler_phase of string

type env_t = (identifier, string) Hashtbl.t

(* Return the latest hash for a given variable variable. Since the variables
 * are stored in the hashtable, a failed lookup means we've seen it zero times so
 * we can just generate a new hash. *)
let get_hash tbl key =
  try Hashtbl.find tbl key
  with _ -> Utils.generate_hash ()

let get_hashed_variable id hash =
  id ^ "_" ^ hash

let rec uniquify_statement (statement : statement) (env : env_t) : statement =
  match statement with
  | VariableDeclaration (id, expression) ->
    let hash = Utils.generate_hash () in
    Hashtbl.replace env id hash;
    let id' = get_hashed_variable id hash in
    let expression' = uniquify_expression expression env in
    VariableDeclaration (id', expression')
  | FunctionDeclaration (name, params, body) ->
    let hash = Utils.generate_hash () in
    let env' = Hashtbl.copy env in
    let name' = get_hashed_variable name hash in
    let params' = List.map (fun param ->
        let hash = Utils.generate_hash () in
        Hashtbl.replace env' param hash;
        get_hashed_variable param hash
      ) params
    in
    let body' = List.map (fun statement -> uniquify_statement statement env') body in
    FunctionDeclaration (name', params', body')
  | Expression expr ->
    let expression' = uniquify_expression expr env in
    Expression expression'
  | Return expr ->
    let expression' = uniquify_expression expr env in
    Return expression'
  | _ -> statement

and uniquify_expression (expression : expression) (env : env_t) : expression =
  match expression with
  | Variable id ->
    let hash = get_hash env id in
    let id' = get_hashed_variable id hash in
    Variable id'
  | BinaryOperation (binop, left, right) ->
    let left' = uniquify_expression left env in
    let right' = uniquify_expression right env in
    BinaryOperation (binop, left', right')
  | Object pairs ->
    let pairs' = List.map (fun pair ->
        let (key, value) = pair in
        let value' = uniquify_expression value env in
        (key, value')
      ) pairs
    in
    Object pairs'
  | Access (e1, e2) ->
    let e1' = uniquify_expression e1 env in
    let e2' = uniquify_property e2 env in
    Access (e1', e2')
  | _ -> expression

and uniquify_property (prop : property) (env : env_t) : property =
  match prop with
  | PropertyExpression expr -> PropertyExpression (uniquify_expression expr env)
  | PropertyIdentifier id -> PropertyIdentifier id

let transform (program : program) : program =
  let env = Hashtbl.create 53 in
  match program with
  | InfraredProgram (statements) ->
    let statements' = List.map (fun statement -> uniquify_statement statement env) statements in
    InfraredProgram statements'
  | _ -> raise (Unexpected_compiler_phase "Expected InfraredProgram")
