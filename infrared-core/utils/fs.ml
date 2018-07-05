
let extract_whitelisted_files files ext =
  List.fold_left (fun acc file ->
      let len = String.length file in
      let ext_len = String.length ext in
      if len > ext_len && String.sub file (len - ext_len) ext_len = ext then
        file :: acc
      else
        acc
    ) [] files

(* Find every file given the name of a file or directory. *)
let extract_files name =
  if Sys.file_exists name = false then
    None
  else
    let rec crawl filename =
      if Sys.file_exists filename = false then
        (Error_handler.report
           ~msg:(Printf.sprintf "Unable to resolve the file: %s" filename)
           ~level:(Level.Low);
         [])
      else
      if Sys.is_directory filename = true then
        let paths = Sys.readdir filename in
        let paths' = Array.to_list paths in
        List.fold_left (fun acc f ->
            (* Path joining could be different for windows? *)
            let absolute_path = Printf.sprintf "%s/%s" filename f in
            (crawl absolute_path) @ acc
          ) [] paths'
      else
        [filename]
    in Some (crawl name)

(* Check the first character to determine is flag or not. *)
let is_flag arg =
  let first = String.get arg 0 in
  if first = '-' then true else false

(* Separate arguments and flags and produce a tuple. *)
let sanitize_args args =
  List.partition is_flag args
