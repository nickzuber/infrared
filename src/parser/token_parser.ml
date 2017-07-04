
open Parser_env
open Ast

let parse tokens = 
  let open Loc in
  let dummy_loc = { line = -1; column = -1 } in
  { loc = dummy_loc; 
    node = IdentifierName "test" }

