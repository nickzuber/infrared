
open Parser_env
open Token
open Ast
open Loc

let working_file = ref "undefined"

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
  let parsing_pop_err = default_pop_error ^ " when parsing an Expression."

  let is_binop = function
    | Equal | NotEqual | StrictEqual | StrictNotEqual | LessThan | LessThanEqual
    | GreaterThan | GreaterThanEqual | LeftShift | RightShift | RightShiftUnsigned
    | Plus | Minus | Mult | Div | Mod | Pow | Comma | LogicalOr | LogicalAnd | Or | Xor
    | And | Bang | Not | Increment | Decrement | Dot | Colon | Ternary | Assignment -> true
    | _ -> false

  (* Note that this takes binary operator tokens and converts them into their binary operator
   * AST node counterpart *if that counterpart exists as an AST binop*. For example, Assignment 
   * is considered a binop but is not represented as a binary operator in this AST. This should
   * be fine though since we would be handling that different anyways. 
   * CompoundAssignmentOperators are also considered to be something different so they aren't 
   * represented here. *)
  let create_binary_operator op_token = Ast.BinaryOperator.(
    match op_token.body with
    | Operator Equal -> Equal
    | Operator NotEqual -> NotEqual
    | Operator StrictEqual -> StrictNotEqual
    | Operator StrictNotEqual -> StrictNotEqual
    | Operator LessThan -> LessThan
    | Operator LessThanEqual -> LessThanEqual
    | Operator GreaterThan -> GreaterThan
    | Operator GreaterThanEqual -> GreaterThanEqual
    | Operator In -> In
    | Operator Instanceof -> Instanceof
    | Operator LeftShift -> LeftShift
    | Operator RightShift -> RightShift
    | Operator RightShiftUnsigned -> RightShiftUnsigned
    | Operator Plus -> Plus
    | Operator Minus -> Minus
    | Operator Mult -> Mult
    | Operator Div -> Div
    | Operator Mod -> Mod
    | Operator Pow -> Pow
    | Operator Comma -> Comma
    | Operator LogicalOr -> LogicalOr
    | Operator LogicalAnd -> LogicalAnd
    | Operator Or -> Or
    | Operator Xor -> Xor
    | Operator And -> And
    | _ -> 
      let msg = "Attempted to create a binary operator with an incompatible token" in
      let err = Error_handler.exposed_error ~source:(!working_file) ~loc:op_token.loc ~msg:msg
      in raise (ParsingError err))

  let create_number_literal ~value token = Ast.LiteralNumericExpression.(
    { _type = "LiteralNumericExpression"; value })

  let create_identifier_literal ~name token = Ast.IdentifierExpression.(
    { _type = "IdentifierExpression"; name })

  let rec parse ?(last_node=None) token_list = Expression.(
    if List.length token_list = 0 then
      match last_node with
      | Some ast_node -> ast_node, token_list
      | None -> let msg = Printf.sprintf
        "Tried to parse an Expression when there were no tokens at all. We never should have \
        even entered the Expression parsing subroutine."
        in raise (ParsingError msg)
    else
      begin
        let token, token_list' = optimistic_pop_token ~err:parsing_pop_err token_list in
        match token.body with
        | Number value -> 
          let ast_node = (LiteralNumericExpression (create_number_literal ~value:value token))
          in parse ~last_node:(Some ast_node) token_list'
        | Identifier name ->
          let ast_node = (IdentifierExpression (create_identifier_literal ~name:name token))
          in parse ~last_node:(Some ast_node) token_list'
        | Operator op when is_binop op ->
          begin
            match last_node with
            | Some left -> 
              (* Pass in initial token_list to preserve the binop *)
              let expr, token_list'' = (parse_binary_expression left token_list) in
              (Expression.BinaryExpression expr), token_list''
            | None -> 
              let msg = "Unexpected binary operator found when trying to parse an expression" in
              let err = Error_handler.exposed_error ~source:(!working_file) ~loc:token.loc ~msg:msg
              in raise (ParsingError err)
          end
        | _ -> 
          match last_node with
          (* We return the initial token_list to preserve the popped token *)
          | Some ast_node -> ast_node, token_list
          | None -> raise (Unimplemented ("Expression_parser.parse -> " ^ (lazy_token_to_string token)))
      end)
  
  and parse_binary_expression left token_list = BinaryExpression.(
    (* Pop the binop *)
    let op_token, token_list' = optimistic_pop_token token_list in
    let operator = create_binary_operator op_token in
    let right, token_list'' = parse token_list'
    in { _type = "BinaryExpression"; left; operator; right }, token_list'')
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
              let init, token_list''' = Expression_parser.parse token_list'' in
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
  working_file := source;
  let directives, tokens = parse_directives starting_tokens in
  (* assuming Module type program *)
  let ast = create_module_ast directives tokens
  in { source; tokens; ast }
