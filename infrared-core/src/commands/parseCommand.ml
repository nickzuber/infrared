open CommandSpec

let generate_ast_list ~args ~flags =
  Printf.printf "Executing 'parse' command...";
  let files = FileParser.get_files_from_args args in
  let files = List.sort (fun a b -> String.compare a b) files in
  if List.length files > 0 then begin
    let ast_list = List.fold_left (fun acc path ->
        let _ = InfraredParser.parse path in
        acc)
        [] files
    in
    ()
  end else
    (Error_handler.report
       ~source:"" ~msg:("No files found with given path") ~level:(Level.Low))

let spec = CommandSpec.create
    ~name:"parse"
    ~doc:"Parses the targeted files and returns a list of the ASTs produced."
    ~flags:[]

let exec = generate_ast_list;
