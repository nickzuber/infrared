
let get_mtime file =
  Unix.((stat file).st_mtime)

let create_cache ?size:(size=50) =
  Hashtbl.create size

(*
   Checks the cache for the last modified time of a given file.
   Few scenarios:
    - File exists
      - Has been updated   => update cache; return true
      - No changes         => return false
    - File does not exist  => add to cache; return true
 *)
let check_diff file ~cache = 
  let file_mtime = get_mtime file in
  try
    let cached_mtime = Hashtbl.find cache file in
    if cached_mtime <> file_mtime then
      (Hashtbl.replace cache file file_mtime; true)
    else 
      false
  with
    Not_found -> 
      (Hashtbl.add cache file file_mtime; true)



