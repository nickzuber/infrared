
open CommandSpec

(* Same as regular type checking, only we consume a JSON parsetree
 * instead of a raw JavaScript file. *)
let check_files ~args ~flags =
  (* let _ = List.exists (fun x -> x = "--flag-we-care-about") flags in *)
  let files = FileParser.get_files_from_args args ~ext:".json" in
  let files = List.sort (fun a b -> String.compare a b) files in
  if List.length files > 0 then
    List.iter (fun path ->
        let _ = Analyzer.check path in
        ())
      files
  else
    (Error_handler.report
       ~msg:("No files found with given path") ~level:(Level.Low))

let spec =
  CommandSpec.create
    ~name:"check-cached"
    ~doc:"Analyzes an already parsed file's JSON parsetree."
    ~flags: []

let exec = check_files;
