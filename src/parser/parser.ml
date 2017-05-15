
module Lex_env = Lexer.Lex_env
module Token = Lexer.Token
open Batteries
open Ast
open Lex_env

let parse file =
  let input = open_in file in
  let filebuf = Lexing.from_input input in
  let env = { Lex_env.new_env with source = file; } in
  let final_env = Lexer.token env filebuf in
  IO.close_in input; (* might not be best spot for this *)
  (* debug final_env; *)
  match final_env.error with
  | Some (msg, lvl) -> 
    (Error_handler.report ~msg:(msg) ~level:(lvl));
    final_env
  | None -> final_env

let print_tokens env =
  Printf.printf "FILE:\t%s\nTOKENS:\n" env.source;
  List.iter (fun tok -> 
    Printf.printf "%s\n"
    (Token.full_token_to_string ~indent:2 tok))
  env.token_list

let print_ast envs =
  List.iter (fun env -> print_tokens env) envs

