
open CommandSpec
open Ast

let generate_ast_list ~args ~flags =
  let files = List.fold_left (fun acc arg -> 
    let response = Fs.extract_files arg in
    match response with
    | Some paths -> acc @ paths
    | None -> acc
  ) [] args in
  List.fold_left (fun acc path ->
    let ast = Parser.parse path in
    acc @ [ast]
  ) [] files

let spec = CommandSpec.create
  ~name:"parse"
  ~doc:"Parses the targeted files and returns a list of the ASTs produced."
  ~flags:[]

let exec = generate_ast_list;
