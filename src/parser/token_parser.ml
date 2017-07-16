
open Parser_env
open Token
open Ast
open Loc

exception ParsingError of string
exception Unimplemented of string

let default_pop_error = "Found no tokens to pop when we were expecting some"
let default_peek_error = "Found no tokens to peek at when we were expecting some"
let default_lookahead_error = "Found no tokens to lookahead when we were expecting some"

(* Pop first token from token list and return the rest of the list.
 * An error is thrown if there are no tokens in the list before we pop. 
 * Parser_env.t -> Token.t * Token.t list *)
let optimistic_pop_token ?(err=default_pop_error) token_list =
  let maybe_tokens = pop token_list in
  let full_token, token_list' = 
    match maybe_tokens with
    | Some tuple -> tuple
    | None -> raise (ParsingError err)
  in full_token, token_list'

let optimistic_peek_token ?(err=default_peek_error) token_list =
  if List.length token_list > 0 then List.hd token_list
  else raise (ParsingError err)

(* Look at nth token with the assumption that it exists. We only want to
 * call this function when we're absolutely sure that we have a token to look
 * at (or are absolutely sure there _should_ be a token we want to look at) *)
let optimistic_lookahead ?(n=1) ?(err=default_lookahead_error) tokens =
  let token = lookahead ~n:n tokens in
  match token with
  | Some token -> token
  | None -> raise (ParsingError err)

(*
  Expression.
    LiteralNumericExpression
    BinaryExpression
*)
module Expression_parser = struct
  let is_binop = function
    | Equal
    | NotEqual
    | StrictEqual
    | StrictNotEqual
    | LessThan
    | LessThanEqual
    | GreaterThan
    | GreaterThanEqual
    | LeftShift
    | RightShift
    | RightShiftUnsigned
    | Plus
    | Minus
    | Mult
    | Div
    | Mod
    | Pow
    | Comma
    | LogicalOr
    | LogicalAnd
    | Or
    | Xor
    | And
    | Bang
    | Not
    | Increment
    | Decrement
    | Dot
    | Colon
    | Ternary
    | Assignment -> true
    | _ -> false

  let create_binary_operator (op: Token.ops) = Ast.BinaryOperator.(
    match op with
    | Equal -> Equal
    | _ -> Equal)

  (* We currently don't actually know the number's value because I don't think we need to.
   * Might be nice to have, so leaving in the option to add it later if we want to. *)
  let create_number_literal token = Ast.LiteralNumericExpression.(
    let value = 0.0
    in { _type = "LiteralNumericExpression"; value })

  let parse_binary_expression left op token_list = BinaryExpression.(
    (* Eat the binop *)
    let token_list' = eat token_list in
    let operator = create_binary_operator op in
    let right = left (* should return a token_list after parsing rhs *)
    in { _type = "BinaryExpression"; operator; left; right }, token_list')

  let rec parse token_list ast_nodes = Expression.(
    let token, token_list' = optimistic_pop_token token_list in
    match token.body with
    (* LiteralNumericExpression, BinaryExpression, CallExpression *)
    | Number when (List.length token_list' > 0) -> 
      begin
        let next_token = optimistic_peek_token token_list' in
        match next_token.body with
        | Operator op when is_binop op -> 
          begin
            let left = create_number_literal token in
            let wrapped_left = LiteralNumericExpression left in
            (*! should add to `ast_nodes` and continue *)
            let expr, token_list'' = (parse_binary_expression wrapped_left op token_list') in
            (Expression.BinaryExpression expr), token_list''
          end
        | _ -> raise (Unimplemented ("Expression_parser.parse: Number -> " ^ (lazy_token_to_string token)))
      end
    | Number -> (LiteralNumericExpression (create_number_literal token)), token_list'
    | String _ -> raise (Unimplemented ("Expression_parser.parse: String -> " ^ (lazy_token_to_string token)))
    | Identifier _ -> raise (Unimplemented ("Expression_parser.parse: Identifier -> " ^ (lazy_token_to_string token)))
    (* this actually implies that we're finished parsing this expression,
     * so we should return `ast_node` here *)
    | _ -> raise (Unimplemented ("Expression_parser.parse -> " ^ (lazy_token_to_string token))))
end

(* 
  Statement.
    VariableDeclarationStatement

  This is because we only ever have a VDS in the content of
  a Statement. Might be worthwhile to implement a Statement_parser
  and parse Variables as thier own thing.
*)
module Variable_parser = struct
  let declarator_err = "Looking for declarators, found none. \
                        Failed to create binding"
  let declarator_pop_err = "Looking for declarators, found no tokens"
  let declarator_op_err = "Looking for declarator list, found illegal operator."

  let create_binding_identifier name = BindingIdentifier.(
    { _type = "BindingIdentifier"; name })

  let create_declarator binding init = VariableDeclarator.(
    { _type = "VariableDeclarator"; binding; init })

  let rec parse_declarators declarators_so_far token_list = 
    let token, token_list' = optimistic_pop_token token_list ~err:declarator_pop_err in
    (* we expect an identifier token *)
    let binding = match token.body with
      | Identifier name -> (create_binding_identifier name)
      | _ -> raise (ParsingError declarator_err) in
    (* check next token to see if we have more identifiers 
     * Pattern match the token body specifiecally here so we can return
     * return an Empty_token if need be *)
    let next_token_body, token_list'' = match pop token_list' with
      | Some (token, new_token_list) -> token.body, new_token_list
      | None -> Empty_Token, token_list' in
    match next_token_body with
      | Operator op ->
        begin
          match op with
          (* done with declarators, parse init. Wrap it up *)
          | Assignment -> 
            begin
              (* @TODO: parse init expression *)
              let init, token_list''' = Expression_parser.parse token_list'' [] in
              let declarator = create_declarator binding (Some init) in
              let updated_declarators = declarator :: declarators_so_far
              in (List.rev updated_declarators), token_list'''
            end
          (* we have more declarators, no init yet *)
          | Comma -> 
            begin
              let declarator = create_declarator binding None in
              let updated_declarators = declarator :: declarators_so_far
              in parse_declarators updated_declarators token_list''
            end
          | _ -> raise (ParsingError declarator_op_err)
        end
      (* No assignment & done with declarators. Wrap it up *) 
      | _ -> 
        begin
          let declarator = create_declarator binding None in
          let updated_declarators = declarator :: declarators_so_far
          in (List.rev updated_declarators), token_list''
        end
    
  let parse_declaration ~t token_list = VariableDeclaration.(
    let t' = match t with
    | Var -> VariableDeclarationKind.Var
    | Let -> VariableDeclarationKind.Let
    | Const -> VariableDeclarationKind.Const
    in let declarators, token_list' = parse_declarators [] token_list in
    let node = { _type = "VariableDeclaration"; kind = t'; declarators }
    in node, token_list')

  let parse_declaration_statement ~t token_list = VariableDeclarationStatement.(
    let declaration, token_list' = parse_declaration ~t:t token_list in
    let node = { _type = "VariableDeclarationStatement"; declaration } in
    let wrapped_node = Ast.Statement.VariableDeclarationStatement node
    in wrapped_node, token_list')
end


(* 
  Module.
    ImportDeclaration
    ExportDeclaration
    Statement 
*)
module Module_parser = struct
  let rec parse_items token_list ast_nodes =
    (* Exit if we're done parsing tokens *)
    if List.length token_list = 0 then ast_nodes else
    (* Steal first token and figure out what to do *)
    let token, token_list' = optimistic_pop_token token_list in
    match token.body with
    | Variable t -> 
      begin
        let node, token_list'' = Variable_parser.parse_declaration_statement ~t:t token_list' in
        let wrapped_node = Ast.Module.Statement node in
        let ast_nodes' = wrapped_node :: ast_nodes in
        parse_items token_list'' ast_nodes'
      end
    | _ -> raise (Unimplemented ("Module_parser.parse_items -> " ^ (lazy_token_to_string token)))
end

(* 
  Program.
    Module
*)
let create_module_ast directives token_list = Module.(
  let items = Module_parser.parse_items token_list [] in
  let node = { _type = "Module"; directives; items; } in
  let wrapped_node = Ast.Program.Module node in
  wrapped_node)

let parse_directives token_list = 
  let rec get_all_directives token_list directives =
    (* No tokens means we're done here *)
    if List.length token_list = 0 then directives, token_list else
    (* Tokens left we still want to check for directives *)
    let first_token = List.hd token_list in
    match first_token.body with
    | String directive -> 
      let token_list' = eat token_list in 
      let directives' = directive :: directives in
      get_all_directives token_list' directives'
    | _ -> directives, token_list in
  let raw_directives, final_tokens = get_all_directives token_list [] in
  let final_directives = 
    List.fold_left 
      (fun acc rawValue -> 
        let dir = { Directive._type = "Directive"; rawValue } in
        dir :: acc)
      [] raw_directives
  in final_directives, final_tokens

(* Initial parser env of a Module or Script *)
let parse starting_tokens source =
  let directives, tokens = parse_directives starting_tokens in
  (* assuming Module type program *)
  let ast = create_module_ast directives tokens
  in { source; tokens; ast }
