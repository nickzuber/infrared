
(* Tab spacing *)
let step = 2
let tab = step * 2

let pp_directive ?(indent=0) node =
  let open Ast.Directive in
  let indentation = String.make indent ' ' in
  let indentation_x2 = String.make (indent + step) ' ' in
  Printf.sprintf
    "%sDirective { \n\
    %srawValue: \"%s\" \n\
    %s},\n"
    indentation
    indentation_x2
    node.rawValue
    indentation

let pp_module ?(indent=0) node =
  let open Ast.Module in
  let indentation = String.make indent ' ' in
  let indentation_x2 = String.make (indent + step) ' ' in
  let directives =
    (List.fold_left
      (fun acc directive -> acc ^ (pp_directive ~indent:(indent + tab) directive))
      "" node.directives) in
  let items = "items"(* match node.items with
  | ImportDeclaration node ->
  | ExportDeclaration node ->
  | Statement node -> *)
  in Printf.sprintf
    "%sModule { \n\
      %sdirectives: [\n%s%s],\n\
      %sitems: [\n%s%s],\n\
    %s}"
    indentation
    indentation_x2
    directives
    indentation_x2
    indentation_x2
    "items\n"
    indentation_x2
    indentation

let pp_program ?(indent=0) node =
  let open Ast.Program in
  let indentation = String.make indent ' ' in
  match node with
  (*| Script node -> pp_script ~indent:indent node*)
  | Module node -> pp_module ~indent:indent node
