
open Parser_env
open Token
open Ast
open Loc

exception ParsingError of string
exception Unimplemented


(* Pop first token from token list and return the rest of the list.
 * An error is thrown if there are no tokens in the list before we pop. 
 * Parser_env.t -> Token.t * Token.t list *)
let optimistic_pop_token ?(err="Found no tokens to parse") token_list =
  let maybe_tokens = pop token_list in
  let full_token, token_list' = 
    match maybe_tokens with
    | Some tuple -> tuple
    | None -> raise (ParsingError err)
  in full_token, token_list'


(* Parsing Modules *)

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

  let rec parse_declarators declarators_so_far token_list = 
    let token, token_list' = optimistic_pop_token token_list ~err:declarator_pop_err in
    (* we expect an identifier token *)
    let binding = match token.body with
      | Identifier name -> (create_binding_identifier name)
      | _ -> raise (ParsingError declarator_err)
    (* check next token to see if we have more identifiers *)
    in let next_token_body = match lookahead token_list' with
      | Some token -> token.body
      | None -> Empty_Token
    in match next_token_body with
      | Operator op ->
        begin
          match op with
          (* done with declarators, parse init. Wrap it up *)
          | Assignment -> 
            begin
              (* @TODO: parse init expression *)
              let init = None in
              let declarator = create_declarator binding init in
              let updated_declarators = declarator :: declarators_so_far in
              updated_declarators, token_list'
            end
          (* we have more declarators, no init yet *)
          | Comma -> 
            begin
              let declarator = create_declarator binding None in
              let updated_declarators = declarator :: declarators_so_far in
              let token_list'' = eat token_list' in
              parse_declarators updated_declarators token_list''
            end
          | _ -> raise (ParsingError declarator_op_err)
        end
      (* No assignment & done with declarators. Wrap it up *) 
      | _ -> 
        begin
          let declarator = create_declarator binding None in
          let updated_declarators = declarator :: declarators_so_far in
          updated_declarators, token_list'
        end
    
  let parse_declaration loc ~t token_list =
    let t' = match t with
    | Var -> VariableDeclarationKind.Var
    | Let -> VariableDeclarationKind.Let
    | Const -> VariableDeclarationKind.Const
    in let declarators, token_list' = parse_declarators [] token_list in
    let node = 
      { VariableDeclaration.
        _type = "VariableDeclaration";
        kind = t';
        declarators }
    in node, token_list'

  let parse loc ~t token_list =
    let declaration, token_list' = parse_declaration loc ~t:t token_list in
    let node = 
      { VariableDeclarationStatement.
        _type = "VariableDeclarationStatement";
        declaration }
    in let wrapped_node = Ast.Statement.VariableDeclarationStatement node in
    wrapped_node, token_list'
end


(* Generic Parsing Functions *)

(* Parses and collects a list of ast nodes *)
let rec module_items_token_parser token_list nodes =
  (* Exit if we're done parsing tokens *)
  if List.length token_list = 0 then nodes else
  (* Steal first token and figure out what to do *)
  let token, token_list' = optimistic_pop_token token_list in
  match token.body with
  | Variable t -> 
    begin
      let node, token_list'' = Variable_parser.parse token.loc ~t:t token_list' in
      let wrapped_node = Ast.Module.Statement node in
      let nodes' = wrapped_node :: nodes in
      module_items_token_parser token_list'' nodes'
    end
  | _ -> raise Unimplemented

let create_module_ast directives token_list = 
  let items = module_items_token_parser token_list [] in
  let node = 
    { Module.
      _type = "Module"; directives; items; }
  in let wrapped_node = Ast.Program.Module node in
  wrapped_node

(*
let create_script_ast directives token_list =
  let statements = generic_token_parser token_list [] in
  let node = 
    { Script.
      _type = "Script"; directives; statements }
  in let wrapped_node = Ast.Program.Script node in
  wrapped_node
*)

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
    | _ -> directives, token_list
  in let raw_directives, final_tokens = get_all_directives token_list [] in
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
  let ast = create_module_ast directives tokens in
  { source; tokens; ast }
