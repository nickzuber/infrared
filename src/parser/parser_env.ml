
open Ast
open Token
open Lex_env

type valid_initial_ast_node = 
  | Module of Module.t
  | Script of Script.t

type t = {
  source: string;
  tokens: Token.t list;
  ast: Module.t;
}

(* Return the nth token in a list *)
let lookahead ?(n=1) tokens =
  try Some (List.nth tokens (n - 1))
  with _ -> None

(* Return next token in env if one exists and the rest of the tokens
 * without the first *)
let pop tokens = 
  match tokens with
  | t :: toks -> Some (t, toks)
  | _ -> None

(* Destroy n tokens from the front of the list *)
let eat ?(n=1) tokens = 
  let rec inner_eat n tokens = 
    match tokens with
    | _ :: toks when n > 0 -> inner_eat (n - 1) toks
    (* Either n has hit zero or no tokens left to eat *)
    | _ -> tokens
  in inner_eat n tokens

let print_single_ast ast =
  Printf.printf "%s\n" "unimplemented"
