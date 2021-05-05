open InfraredUtils
open InfraredParser.Ast
open InfraredAst

exception Unexpected_compiler_phase of string

type env_t = (identifier', string) Hashtbl.t

(* Return the latest hash for a given variable variable. Since the variables
 * are stored in the hashtable, a failed lookup means this variable was never
 * declared, so there's a chance its a global. *)
let get_hash (tbl : env_t) (key : identifier') : string =
  try Hashtbl.find tbl key
  with _ -> ""

let get_hashed_variable id hash =
  match hash with
  | "" -> id
  | _ -> id ^ "_" ^ hash

let rec uniquify_statements (statements : statement list) (env : env_t) : statement list =
  let statements' = List.map (fun s -> uniquify_function_declaration_statement s env) statements in
  let statements'' = List.map (fun s -> uniquify_all_other_statement s env) statements' in
  statements''

and uniquify_function_declaration_statement (statement : statement) (env : env_t) : statement =
  let (loc, statement) = statement in
  match statement with
  | FunctionDeclaration ((name_loc, name), params, body) ->
    let hash = Utils.generate_hash () in
    Hashtbl.replace env name hash;
    let name' = get_hashed_variable name hash in
    (* @NOTE: Environment Copying
     * Copying the environment for the function body is what lets us remove
     * variable shadowing. This is because we won't overwrite any variables
     * in the current environment, since this inner closure is contained. *)
    let env' = Hashtbl.copy env in
    let params' = List.map (fun (param_loc, param) ->
        let hash = Utils.generate_hash () in
        Hashtbl.replace env' param hash;
        let param' = get_hashed_variable param hash in
        (param_loc, param')
      ) params
    in
    let body' = uniquify_statements body env' in
    (loc, FunctionDeclaration ((name_loc, name'), params', body'))
  | _ -> (loc, statement)

and uniquify_all_other_statement (statement : statement) (env : env_t) : statement =
  let (loc, statement) = statement in
  match statement with
  | VariableDeclaration ((id_loc, id), expression) ->
    let hash = Utils.generate_hash () in
    Hashtbl.replace env id hash;
    let id' = get_hashed_variable id hash in
    let expression' = uniquify_expression expression env in
    (loc, VariableDeclaration ((id_loc, id'), expression'))
  | Expression expr ->
    let expression' = uniquify_expression expr env in
    (loc, Expression expression')
  | Return expr ->
    let expression' = uniquify_expression expr env in
    (loc, Return expression')
  | Block statements ->
    (* @NOTE: Block Scopes
     * @SEE: Environment Copying
     * Technically block scopes are only applied with `let` or `const` variables.
     * I should eventually come back to clear this up, but for now let's ignore it
     * and assume all variables are declared this way.
     * https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/block#block_scoping_rules_with_var_or_function_declaration_in_non-strict_mode *)
    let env' = Hashtbl.copy env in
    let statements' = uniquify_statements statements env' in
    (loc, Block statements')
  | If (expr, s1, s2) ->
    let expr' = uniquify_expression expr env in
    let s1' = uniquify_all_other_statement s1 env in
    let s2' = uniquify_all_other_statement s2 env in
    (loc, If (expr', s1', s2'))
  | FunctionDeclaration (name, args, body) ->
    (loc, FunctionDeclaration (name, args, body))

and uniquify_expression (expression : expression) (env : env_t) : expression =
  let (loc, expression) = expression in
  match expression with
  | Variable (id_loc, id) ->
    let hash = get_hash env id in
    let id' = get_hashed_variable id hash in
    (loc, Variable (id_loc, id'))
  | BinaryOperation (binop, left, right) ->
    let left' = uniquify_expression left env in
    let right' = uniquify_expression right env in
    (loc, BinaryOperation (binop, left', right'))
  | Object pairs ->
    let pairs' = List.map (fun pair ->
        let (key, value) = pair in
        let value' = uniquify_expression value env in
        (key, value')
      ) pairs
    in
    (loc, Object pairs')
  | Access (e1, e2) ->
    let e1' = uniquify_expression e1 env in
    let e2' = uniquify_property e2 env in
    (loc, Access (e1', e2'))
  | Call (callee, args) ->
    let callee' = uniquify_expression callee env in
    let args' = List.map (fun arg -> uniquify_expression arg env) args in
    (loc, Call (callee', args'))
  | _ -> (loc, expression)

and uniquify_property (prop : property) (env : env_t) : property =
  match prop with
  | PropertyExpression expr -> PropertyExpression (uniquify_expression expr env)
  | PropertyIdentifier id -> PropertyIdentifier id

let transform (program : program) : program =
  let env = Hashtbl.create 53 in
  match program with
  | InfraredProgram (statements) ->
    let statements' = uniquify_statements statements env in
    InfraredProgram statements'
  | _ -> raise (Unexpected_compiler_phase "Expected InfraredProgram")
