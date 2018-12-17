
open CommandSpec

let generate_token_list ~args ~flags =
  let files = FileParser.get_files_from_args args in
  if List.length files > 0 then
    List.fold_left (fun acc path -> acc)
      [] files
  else
    (Error_handler.report
       ~source:"" ~msg:("No files found with given path") ~level:(Level.Low); [])

let spec = CommandSpec.create
    ~name:"tokenize"
    ~doc:"Tokenizes the targeted files and returns a list of the token lists produced."
    ~flags:[]

let exec = generate_token_list;
