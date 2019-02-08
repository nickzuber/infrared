let blacklist =
  [| ".git"
   ; "node_modules"
   ; "_build"
  |]

let whitelist =
  [| ".js"
   ; ".jsx"
  |]

let is_blacklisted path =
  Array.exists (fun blk -> Utils.is_substring blk path) blacklist

let is_whitelisted file =
  Array.exists (fun wht -> wht = Filename.extension file) whitelist

let rec files_from_path (path : string) : string list =
  let is_dir = Sys.is_directory path in
  let should_halt = is_blacklisted path in
  match (should_halt, is_dir) with
  | (true, _) -> []
  | (false, true) ->
    let inner_paths : string array = Sys.readdir path in
    Array.fold_left
      (fun files inner_path ->
        let absolute_path = Filename.concat path inner_path in
        files @ (files_from_path absolute_path))
      [] inner_paths
  | (false, false) ->
    let should_include = is_whitelisted path in
    if should_include then
      [path]
    else
      []
