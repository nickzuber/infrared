
module Lex_env = Lexer.Lex_env
module Token = Lexer.Token
module Ast = Ast.Ast
open Batteries
open Ast
open Lex_env

let parse file = 
  { Ast.defaultEnv with 
    file = file;
    ast = [Bool; Bool; Bool; Bool; Bool] }

let print_tokens env =
  Printf.printf "
FILE:
\t%s
AST:\n" env.file;
  List.iter (fun tok -> 
    Printf.printf "\t%s\n"
    (Token.token_to_string tok))
  env.ast;
  print_endline ""


let print_ast envs =
  List.iter (fun env -> print_tokens env) envs

