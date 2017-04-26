
(* Default file extensions we want to look at. *)
let whitelist = ["js"]

(* Find every white listed file given the name of a file or directory. *)
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

