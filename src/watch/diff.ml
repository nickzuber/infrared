
let is_file file =
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
 * This function is intended to be used in the context of the
 * `Hashtbl.iter` method, therefore are parameters will be the
 * `key` and `value` which is our file name and m_time. *)
let has_diff file cached_mtime = 
  let current_mtime = get_mtime file in
  if cached_mtime <> current_mtime then
      (Hashtbl.replace cache file file_mtime; true)
  else 
    false

