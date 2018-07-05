
open CommandSpec

(* Since only programs without any parsing errors will be captured here, our
   list of ASTs will only include valid ASTs. This won't matter much once we implement
   typecheck, since we won't be calling Parser.parse anymore. *)
let check_files ~args ~flags =
  (* let _ = List.exists (fun x -> x = "--flag-we-care-about") flags in *)
  let files = FileParser.get_files_from_args args in
  let files = List.sort (fun a b -> String.compare a b) files in
  if List.length files > 0 then
    List.iter (fun path ->
        let maybe_ast = Parser.parse path in  (* We won't "parse" here, but rather "typecheck". this method will call parse *)
        match maybe_ast with
        | Some ast -> Parser.print_typecheck (ast, path)
        | None -> ())
      files
  else
    (Error_handler.report
       ~msg:("No files found with given path") ~level:(Level.Low))

let spec =
  let flags = Flag.create_list [
      ("--watch", "Type checks the target files in real time as you edit.")
    ] in
  CommandSpec.create
    ~name:"check"
    ~doc:"Type checks the targeted files and reports back with any errors found."
    ~flags: flags

let exec = check_files;
