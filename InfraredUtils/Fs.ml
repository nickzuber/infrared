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

let is_directory_opt path : bool option =
  try Some (Sys.is_directory path)
  with Sys_error _ -> None

let rec files_from_path (path : string) : string list =
  let is_dir_opt = is_directory_opt path in
  let is_invalid_file = is_blacklisted path in
  match (is_invalid_file, is_dir_opt) with
  | (true, _) -> []
  | (false, None) -> []
  | (false, Some true) ->
    let inner_paths : string array = Sys.readdir path in
    Array.fold_left
      (fun files inner_path ->
         let absolute_path = Filename.concat path inner_path in
         files @ (files_from_path absolute_path))
      [] inner_paths
  | (false, Some false) ->
    let should_include = is_whitelisted path in
    if should_include then
      [path]
    else
      []

let read_file (file : string) : string =
  let ic = open_in file in
  let len = in_channel_length ic in
  let str = Bytes.create len in
  really_input ic str 0 len;
  close_in ic;
  str
