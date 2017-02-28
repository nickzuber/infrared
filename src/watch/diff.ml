
let is_valid_file file =
  try
    let stat = Unix.stat file in 
    true (* We don't care about the stats, just the fact it found some *)
  with
    Unix.Unix_error (Unix.ENOENT, "stat", _) ->
      false

(* We assume that we have a valid file path. Bad file paths should
 * be handled somewhere else. *)
let get_mtime file =
  Some Unix.((stat file).st_mtime)

(* A default size of 50 seems reasonable at the moment. 
 * Might change later. *)
let create_cache ?size:(size=50) =
  Hashtbl.create size

(* Checks the cache for the last modified time of a given file.
 * Few scenarios:
 *  - File exists
 *    - Has been updated   => update cache; return true
 *    - No changes         => return false
 *  - File does not exist  => add to cache; return true *)
let has_diff file ~cache = 
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



