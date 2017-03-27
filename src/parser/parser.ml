
module Lex_env = Lexer.Lex_env
module Token = Lexer.Token
module Infrared_ast = Ast.Infrared_ast
open Batteries
open Infrared_ast
open Lex_env

let parse file = 
  (* temp for testing right now *)
  let open Token in
  { Infrared_ast.defaultEnv with 
    file = file;
    ast = [Bool; Bool; Bool; Bool; Bool] }

let print_tokens env =
  Printf.printf "
FILE:\t%s
AST:" env.file;
  List.iter (fun tok -> 
    Printf.printf "\t%s\n"
    (Token.token_to_string tok))
  env.ast;
  print_endline "(end)"


let print_ast envs =
  List.iter (fun env -> print_tokens env) envs

