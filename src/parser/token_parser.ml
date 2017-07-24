
open Parser_env
open Token
open Ast
open Loc

let working_file = ref "undefined"

exception ParsingError of string
exception Unimplemented of string

let default_pop_error = "Found no tokens to pop when we were expecting some."
let default_peek_error = "Found no tokens to peek at when we were expecting some."
let default_lookahead_error = "Found no tokens to lookahead when we were expecting some."

(* Pop first token from token list and return the rest of the list.
 * An error is thrown if there are no tokens in the list before we pop. *)
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
  Module
*)
module rec Module_parser : sig
  val parse_items : Token.t list -> Ast.Module.item list -> Ast.Module.item list
end = struct
  let rec parse_items token_list ast_nodes =
    (* Exit if we're done parsing tokens *)
    if List.length token_list = 0 then ast_nodes else
    (* Steal first token and figure out what to do *)
    let token, token_list' = optimistic_pop_token token_list in
    match token.body with
    (*| Identifier _*)
    | Variable _ -> 
      (*let node, token_list'' = Variable_parser.parse_declaration_statement ~t:t token_list' in*)
      let node, token_list'' = Statement_parser.parse token_list in
      let wrapped_node = Ast.Module.Statement node in
      let ast_nodes' = wrapped_node :: ast_nodes in
      parse_items token_list'' ast_nodes'
    (* @TODO no need to explicitly implement skippable tokens, this will be handled by the catchall 
       at the end of the match statement once everything is implemented. *)
    | Comment -> parse_items token_list' ast_nodes
    | _ ->
      let tok = lazy_token_to_string token in
      let msg = "We haven't implemented a way to parse this token yet in Module items: " ^ tok in
      let err = Error_handler.exposed_error ~source:(!working_file) ~loc:token.loc ~msg:msg in
      raise (Unimplemented err)
end



(*
  Statement
*)
and Statement_parser : sig
  val parsing_pop_err : string
  val parse : Token.t list -> Ast.Statement.t * Token.t list
end = struct
  let parsing_pop_err = default_pop_error ^ " when parsing a Statement."

  let parse token_list =    
    let token, token_list' = optimistic_pop_token ~err:parsing_pop_err token_list in
    match token.body with
    | Variable t ->
      let node, token_list'' = Variable_parser.parse_declaration_statement ~t:t token_list' in
      let ast_node = Ast.Statement.VariableDeclarationStatement node
      in ast_node, token_list''
    | _ -> raise (Unimplemented "ahh")

end



(*
  Expression
*)
and Expression_parser : sig
  val parsing_pop_err : string
  val is_binop : Token.ops -> bool
  val create_binary_operator : Token.t -> Ast.BinaryOperator.t
  val create_number_literal : value:float -> 'a -> Ast.LiteralNumericExpression.t
  val create_identifier_literal : name:Ast.Identifier.t -> 'a -> Ast.IdentifierExpression.t
  val create_call_expression : callee:Ast.CallExpression.callee -> arguments:Ast.Arguments.t -> 'a -> Ast.CallExpression.t
  val create_boolean_literal : 'a -> Ast.LiteralBooleanExpression.t
  val create_spread_element : expression:Ast.Expression.t -> 'a -> Ast.SpreadElement.t
  val parse : ?early_bail_token:Token.t' option -> Token.t list -> Ast.Expression.t * Token.t list
  val parse_rest_of_expression : ?early_bail_token:Token.t' option -> Ast.Expression.t -> Token.t list -> Ast.Expression.t * Token.t list
  val parse_arguments : ?arguments_so_far:Ast.Arguments.arguments list -> Token.t list -> Ast.Arguments.arguments list
  val parse_binary_expression : Ast.Expression.t -> Token.t list -> Ast.BinaryExpression.t * Token.t list
end = struct
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

  let create_call_expression ~callee ~arguments token = Ast.CallExpression.(
    { _type = "CallExpression"; callee; arguments })

  let create_boolean_literal token = Ast.LiteralBooleanExpression.(
    { _type = "LiteralBooleanExpression"; value = true })

  let create_spread_element ~expression token = Ast.SpreadElement.(
    { _type = "SpreadElement"; expression })

  (* Initial parsing phase for an expression. Once we resolve some expression, we want to pass it
   * in to parse_rest_of_expression to see if there's any more we want to do to it. 
   * For example, consider the expression `foo()`. On our initial parsing phase, we'd parse the expression
   * of `foo` as an identifier. We then go on to see if there's something that could augment
   * this expression, and in this case there is and it becomes a call expression of `foo`. *)
  let rec parse ?(early_bail_token=None) token_list = Expression.(
    let token, token_list' = optimistic_pop_token ~err:parsing_pop_err token_list in
    match token.body with
    | Number value -> 
      let ast_node = (LiteralNumericExpression (create_number_literal ~value:value token))
      in parse_rest_of_expression ~early_bail_token:early_bail_token ast_node token_list'
    | Identifier name ->
      let ast_node = (IdentifierExpression (create_identifier_literal ~name:name token))
      in parse_rest_of_expression ~early_bail_token:early_bail_token ast_node token_list'
    | Bool -> 
      let ast_node = (LiteralBooleanExpression (create_boolean_literal token))
      in parse_rest_of_expression ~early_bail_token:early_bail_token ast_node token_list'
    | Expression inner_token_list ->
      let ast_node, _ = parse inner_token_list
      in parse_rest_of_expression ~early_bail_token:early_bail_token ast_node token_list'
    | _ -> 
      let msg = "While parsing an expression, we ran into a token we didn't know what to do with." in
      let err = Error_handler.exposed_error ~source:(!working_file) ~loc:token.loc ~msg:msg
      in raise (Unimplemented err))
  
  and parse_rest_of_expression ?(early_bail_token=None) last_node token_list = Expression.(
    if List.length token_list = 0 then
      last_node, token_list
    else
      begin
        let token, token_list' = optimistic_pop_token ~err:parsing_pop_err token_list in
        (* See if we want to bail out of this method early.
           NOTE: Early bailing currently eats the bailed token. *)
        match early_bail_token with
        | Some bail_token when bail_token = token.body -> last_node, token_list'
        | _ -> 
          begin
            match token.body with
            | Operator op when is_binop op ->
                (* Pass in initial token_list to preserve the binop *)
                let expr, token_list'' = (parse_binary_expression last_node token_list) in
                parse_rest_of_expression (Expression.BinaryExpression expr) token_list''
            | Expression inner_token_list ->
              let callee = Ast.CallExpression.Expression last_node in
              let arguments = parse_arguments inner_token_list in
              let arguments' = List.rev arguments in
              let ast_node = (CallExpression (create_call_expression ~callee:callee ~arguments:arguments' token))
              in ast_node, token_list'
            (* Since the next token isn't something that could augment this expression, 
            * we know that this expression is finished. *)
            | _ -> last_node, token_list
          end
      end)

  and parse_arguments ?(arguments_so_far=[]) token_list = Arguments.(
    if List.length token_list = 0 then
      arguments_so_far
    else
      begin
        let bail_token = Some (Operator Comma) in
        let token, token_list' = optimistic_pop_token ~err:parsing_pop_err token_list in
        (* Can be either a SpreadElement or Expression *)
        let ast_node, token_list'' =
          match token.body with
          (* SpreadElement *)
          | Spread ->
            let expression, token_list'' = parse ~early_bail_token:bail_token token_list' in
            let ast_node = create_spread_element ~expression:expression token in
            (Ast.Arguments.SpreadElement ast_node), token_list''
          (* Expression *)
          | _ -> 
            (* Pass in original token_list to preserve the token we popped *)
            let ast_node, token_list'' = parse ~early_bail_token:bail_token token_list in
            (Ast.Arguments.Expression ast_node), token_list'' in
        let arguments_so_far' = ast_node :: arguments_so_far
        in parse_arguments ~arguments_so_far:arguments_so_far' token_list''
      end)

  and parse_binary_expression left token_list = BinaryExpression.(
    (* Pop the binop *)
    let op_token, token_list' = optimistic_pop_token token_list in
    let operator = create_binary_operator op_token in
    let right, token_list'' = parse token_list'
    in { _type = "BinaryExpression"; left; operator; right }, token_list'')
end



(* 
  VariableDeclarationStatement
*)
and Variable_parser : sig 
  val create_binding_identifier : Ast.Identifier.t -> Ast.BindingIdentifier.t
  val create_declarator : Ast.BindingIdentifier.t -> Ast.Expression.t option -> Ast.VariableDeclarator.t
  val parse_declarators : Ast.VariableDeclarator.t list -> Token.t list -> Ast.VariableDeclarator.t list * Token.t list
  val parse_declaration : t:Token.var_t -> Token.t list -> Ast.VariableDeclaration.t * Token.t list
  val parse_declaration_statement : t:Token.var_t -> Token.t list -> Ast.VariableDeclarationStatement.t * Token.t list
end = struct
  let declarator_err = "Looking for an identifier, but we found something else instead."
  let declarator_pop_err = "Looking for declarators, found no tokens."
  let declarator_op_err = "Looking for declarator list, found illegal operator."

  let create_binding_identifier name = BindingIdentifier.(
    { _type = "BindingIdentifier"; name })

  let create_declarator binding init = VariableDeclarator.(
    { _type = "VariableDeclarator"; binding; init })

  let rec parse_declarators declarators_so_far token_list = 
    let token, token_list' = optimistic_pop_token token_list ~err:declarator_pop_err in
    let binding = match token.body with
      | Identifier name -> (create_binding_identifier name)
      | _ -> 
        let err = Error_handler.exposed_error ~source:(!working_file) ~loc:token.loc ~msg:declarator_err
        in raise (ParsingError err) in
    (* Check next token to see if we have more identifiers.
     * Pattern match the token body specifiecally here so we can return an Empty_token if need be *)
    let next_token_body, token_list'' = match pop token_list' with
      | Some (token, new_token_list) -> token.body, new_token_list
      | None -> Empty_Token, token_list' in
    match next_token_body with
      | Operator op ->
        begin
          match op with
          (* Done with declarators, parse init. Wrap it up *)
          | Assignment -> 
            begin
              let init, token_list''' = Expression_parser.parse token_list'' in
              let declarator = create_declarator binding (Some init) in
              let updated_declarators = declarator :: declarators_so_far
              in (List.rev updated_declarators), token_list'''
            end
          (* We have more declarators, no init yet *)
          | Comma -> 
            begin
              let declarator = create_declarator binding None in
              let updated_declarators = declarator :: declarators_so_far
              in parse_declarators updated_declarators token_list''
            end
          | _ -> 
            let err = Error_handler.exposed_error ~source:(!working_file) ~loc:token.loc ~msg:declarator_op_err
            in raise (ParsingError err)
        end
      (* No assignment & done with declarators. Wrap it up *) 
      | _ -> 
        let declarator = create_declarator binding None in
        let updated_declarators = declarator :: declarators_so_far
        in (List.rev updated_declarators), token_list'
    
  let parse_declaration ~t token_list = VariableDeclaration.(
    let t' = match t with
      | Var -> VariableDeclarationKind.Var
      | Let -> VariableDeclarationKind.Let
      | Const -> VariableDeclarationKind.Const in
    let declarators, token_list' = parse_declarators [] token_list in
    let node = { _type = "VariableDeclaration"; kind = t'; declarators }
    in node, token_list')

  let parse_declaration_statement ~t token_list = VariableDeclarationStatement.(
    let declaration, token_list' = parse_declaration ~t:t token_list in
    let node = { _type = "VariableDeclarationStatement"; declaration }
    in node, token_list')
end



(* 
  Program
  TODO: make module Program
*)
and Program : sig
  val create_module_ast : Ast.Directive.t list -> Token.t list -> Ast.Program.t
  val parse_directives : Token.t list -> Ast.Directive.t list * Token.t list
  val parse : Token.t list -> string -> Parser_env.t
end = struct
  let create_module_ast directives token_list = Module.(
    let items = Module_parser.parse_items token_list [] in
    let items = List.rev items in
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
end
