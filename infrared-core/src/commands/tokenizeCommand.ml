
open CommandSpec
open Ast

let generate_token_list ~args ~flags =
  let files = FileParser.get_files_from_args args in
  if List.length files > 0 then
    List.fold_left (fun acc path ->
        let maybe_ast = Parser.tokenize path in
        match maybe_ast with
        | Some ast -> acc @ [ast]
        | None -> acc)
      [] files
  else
    (Error_handler.report
       ~source:"" ~msg:("No files found with given path") ~level:(Level.Low); [])

let spec = CommandSpec.create
    ~name:"tokenize"
    ~doc:"Tokenizes the targeted files and returns a list of the token lists produced."
    ~flags:[]

let exec = generate_token_list;
