
open Ast
open Token
open Lex_env

type t = {
  source: string;
  env: Lex_env.t;
  ast: Ast.t;
}

(* Return the next token in env if one exists *)
let peak env =
  match env.token_list with
  | _ :: toks -> Some (List.hd toks)
  | _ -> None

let node_to_string node indent = 
  match node with
  | IdentifierName name -> "IdentifierName " ^ name

let full_ast_to_string ?(indent=0) ast =
  let open Loc in
  Printf.sprintf "%s \x1b[90m(%d:%d)\x1b[39m"
    (node_to_string ast.node indent)
    ast.loc.line
    ast.loc.column

let print_single_ast ast =
  Printf.printf "%s\n" (full_ast_to_string ~indent:2 ast)
