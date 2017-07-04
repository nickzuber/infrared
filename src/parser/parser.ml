
open Batteries
open Ast
open Lex_env

let tokenize file =
  let input = open_in file in
  let filebuf = Lexing.from_input input in
  let env = { Lex_env.new_env with source = file; } in
  let final_env = Lexer.token env filebuf in
  IO.close_in input;
  (* debug final_env; *)
  match final_env.error with
  | Some (msg, lvl) -> 
    (Error_handler.report ~msg:(msg) ~level:(lvl));
    final_env
  | None -> final_env

let print_single_token_list env =
  Printf.printf "FILE:\t%s\nTOKENS:\n" env.source;
  List.iter (fun tok -> 
    Printf.printf "%s\n"
    (Token.full_token_to_string ~indent:2 tok))
  (List.rev env.token_list)

let print_tokens envs =
  List.iter (fun env -> print_single_token_list env) envs

let print_ast asts =
  let open Parser_env in
  List.iter (fun ast -> print_single_ast ast) asts

let parse file =
  let lex_env = tokenize file in
  Token_parser.parse lex_env.token_list

