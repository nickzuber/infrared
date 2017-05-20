
open CommandSpec
open Ast

let generate_token_list ~args ~flags =
  let files = List.fold_left (fun acc arg -> 
    let response = Fs.extract_files arg in
    match response with
    | Some paths -> acc @ paths
    | None -> acc
  ) [] args in
  List.fold_left (fun acc path ->
    let ast = Parser.tokenize path in
    acc @ [ast]
  ) [] files

let spec = CommandSpec.create
  ~name:"tokenize"
  ~doc:"Tokenizes the targeted files and returns a list of the token lists produced."
  ~flags:[]

let exec = generate_token_list;
