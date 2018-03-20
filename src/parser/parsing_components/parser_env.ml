
open Ast
open Token
open Lex_env

type t = {
  source: string;
  tokens: Token.t list;
  ast: Program.t;
}

(* Return the nth token ahead of the first in a list
   Examples:
     lookahead [1;2;3]        ->   Some 2
     lookahead [1;2;3] ~n:2   ->   Some 3
     lookahead [1;2;3] ~n:0   ->   Some 1
     lookahead [1;2;3] ~n:3   ->   None
*)
let lookahead ?(n=1) token_list =
  try Some (List.nth token_list n)
  with _ -> None

let peek token_list =
  try Some (List.hd token_list)
  with _ -> None

(* Return next token in env if one exists and the rest of the tokens without the first *)
let pop token_list =
  match token_list with
  | token :: token_list -> Some (token, token_list)
  | _ -> None

(* Destroy n tokens from the front of the list *)
let eat ?(n=1) token_list =
  let rec inner_eat n token_list =
    match token_list with
    | _ :: toks when n > 0 -> inner_eat (n - 1) toks
    (* Either n has hit zero or no tokens left to eat *)
    | _ -> token_list
  in inner_eat n token_list

let print_single_ast env =
  (* We will always start at parsing a Ast.Program if we're parsing right *)
  Printf.printf "%s\n" (Ast_printer.pp_program env.ast)
