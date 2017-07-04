
open Ast

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

let ast_to_string ast =

let full_node_to_string =
  let open Loc in
  Printf.sprintf "%s \x1b[90m(%d:%d)\x1b[39m"
    (node_to_string node.body indent)
    tok.loc.line
    tok.loc.column