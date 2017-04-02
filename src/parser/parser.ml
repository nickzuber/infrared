
module Lex_env = Lexer.Lex_env
module Token = Lexer.Token
open Batteries
open Ast
open Lex_env

let parse file = 
  (* temp for testing right now *)
  let open Token in
  { Lex_env.defaultEnv with 
    source = file;
    ast = [Bool; Bool; Bool; Bool; Bool] }

let print_tokens env =
  Printf.printf "\nFILE:\t%s\nTOKENS:" env.source;
  List.iter (fun tok -> 
    Printf.printf "\t%s\n"
    (Token.token_to_string tok))
  env.ast;
  print_endline "(end)"


let print_ast envs =
  List.iter (fun env -> print_tokens env) envs

