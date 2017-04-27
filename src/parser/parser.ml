
module Lex_env = Lexer.Lex_env
module Token = Lexer.Token
open Batteries
open Ast
open Lex_env
exception Bad_larry
let parse file =
  let input = open_in file in
  let filebuf = Lexing.from_input input in
  let env = { Lex_env.defaultEnv with source = file; } in
  let final_env = Lexer.token env filebuf in
  debug final_env;
  match final_env.error with
  | Some (msg, lvl) -> 
    (Error_handler.report ~msg:(msg) ~level:(lvl));
    final_env
  | None -> final_env

let print_tokens env =
  Printf.printf "FILE:\t%s\nTOKENS:" env.source;
  List.iter (fun tok -> 
    Printf.printf "\t%s\n"
    (Token.full_token_to_string tok))
  env.ast

let print_ast envs =
  List.iter (fun env -> print_tokens env) envs

