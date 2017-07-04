
open Ast
open Token
open Lex_env

type valid_starting_nodes =
  | Module of Module.t
  | Script of Script.t

type t = {
  source: string;
  env: Lex_env.t;
  ast: valid_starting_nodes;
}

(* Return the next token in env if one exists *)
let peak env =
  match env.token_list with
  | _ :: toks -> Some (List.hd toks)
  | _ -> None

let node_to_string node indent = 
  match node with
  | IdentifierName name -> "IdentifierName " ^ name
  | _ -> "Unknown node"

let full_node_to_string ?(indent=0) node =
  let open Loc in
  Printf.sprintf "%s \x1b[90m(%d:%d)\x1b[39m"
    (node_to_string node indent)
    node.loc.line
    node.loc.column

let print_single_ast ast =
  List.iter (fun node -> 
    Printf.printf "%s\n"
    (full_node_to_string ~indent:2 node))
