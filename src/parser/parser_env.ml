
open Ast
open Token
open Lex_env

type t = {
  source: string;
  tokens: Token.t list;
  ast: Ast.t;
}

(* Return the next token in env if one exists *)
let peek env =
  match env.tokens with
  | _ :: toks -> Some (List.hd toks)
  | _ -> None

(* Return next token in env if one exists and the rest of the tokens
 * without the first *)
let pop env = 
  match env.tokens with
  | t :: toks -> 
    begin 
      let env' = { env with tokens = toks } in
      Some (t, env')
    end
  | _ -> None

(* Destroy n tokens from the front of the list *)
let eat ?(n=1) env = 
  let rec inner_eat n env = 
    match env.tokens with
    | _ :: toks when n > 0 -> 
      begin
        let env' = { env with tokens = toks } in
        inner_eat (n - 1) env'
      end
    (* Either n has hit zero or no tokens left to eat *)
    | _ -> env
  in inner_eat n env

(* @TODO 
 * figure out this shit *)
let node_to_string node i = 
  let indentation = String.make i ' ' in
  let step = 4 in
  match node with
  | _ -> indentation ^ "-"

let full_node_to_string ?(indent=0) ast =
  let open Loc in
  Printf.sprintf "%s \x1b[90m(%d:%d)\x1b[39m"
    (node_to_string ast.node indent)
    ast.loc.line
    ast.loc.column

let print_single_ast ast =
  Printf.printf "%s\n" (full_node_to_string ~indent:2 ast)
