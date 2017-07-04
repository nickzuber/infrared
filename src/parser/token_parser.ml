
open Parser_env
open Token
open Ast
open Loc

exception ParsingError of string


(* Pop first token from token list and return the rest of the list.
 * An error is thrown if there are no tokens in the list before we pop. 
 * Parser_env.t -> Token.t * Token.t list *)
let safe_pop_token ?(err="Found no tokens to parse") env =
  let maybe_tokens = pop env in
  let full_token, env' = 
    match maybe_tokens with
    | Some tuple -> tuple
    | None -> raise (ParsingError err)
  in full_token, env'


module Variable_parser = struct
  let declarator_err = "Looking for declarators, found none. \
                        Failed to create binding"
  let declarator_pop_err = "Looking for declarators, found no tokens"
  let declarator_op_err = "Looking for declarator list, found illegal operator."

  let create_binding_identifier name = 
    { BindingIdentifier.
      _type = "BindingIdentifier"; name }

  let create_declarator binding init = 
    { VariableDeclarator.
      _type = "VariableDeclarator";
      binding; init }

  let rec parse_declarators declarators_so_far env = 
    let full_token, env' = safe_pop_token env ~err:declarator_pop_err in
    let token = full_token.body in
    let loc = full_token.loc in
    let next_token_body = match peek env' with
      | Some token -> token.body
      | None -> Nil_Token
    in let binding = match token with
      | Identifier name -> (create_binding_identifier name)
      | _ -> raise (ParsingError declarator_err)
    in let continue_status = match next_token_body with
      | Operator op ->
        begin
          match op with
          (* done with declarators, parse init. Wrap it up *)
          | Assignment -> 0
          (* we have more declarators, no init *)
          | Comma -> 1
          | _ -> raise (ParsingError declarator_op_err)
        end
      (* No assignment & done with declarators. Wrap it up *) 
      | _ -> 2
    in 
      if continue_status = 1 then
        begin
          let declarator = create_declarator binding None in
          let updated_declarators = declarator :: declarators_so_far in
          let env'' = eat env' in
          parse_declarators updated_declarators env''
        end
      else
        if continue_status = 0 then
          begin
            let init = None in
            let declarator = create_declarator binding init in
            declarator :: declarators_so_far
          end
        else
          begin
            let declarator = create_declarator binding None in
            declarator :: declarators_so_far
          end

  let parse_declaration ~t env =
    let t' = match t with
    | Var -> VariableDeclarationKind.Var
    | Let -> VariableDeclarationKind.Let
    | Const -> VariableDeclarationKind.Const
    in { VariableDeclaration.
        _type = "VariableDeclaration";
        kind = t';
        declarators = parse_declarators [] env }

  let parse token loc ~t env =
    { VariableDeclarationStatement.
      _type = "VariableDeclarationStatement";
      declaration = parse_declaration ~t:t env }
end

let init_env tokens source =
  let faux_ast_loc = { line = -1; column = -1 } in
  let faux_ast_element = { loc = faux_ast_loc; node = Nil } in
  { source; tokens; ast = faux_ast_element }

let generic_token_parser full_token env =
  let token = full_token.body in
  let loc = full_token.loc in
  match token with
  | Variable t -> 
    begin
      let raw_node = Variable_parser.parse token loc ~t:t env in
      let node = VariableDeclarationStatement raw_node in
      (* no need for patternable wrapper
         just pattern match again _type names since they will be set *)
      { loc; node }
    end
  | _ -> { loc; node = Ast.Nil }

let parse tokens source = 
  let env = init_env tokens source in 
  let full_token, env' = safe_pop_token env in
  generic_token_parser full_token env'
