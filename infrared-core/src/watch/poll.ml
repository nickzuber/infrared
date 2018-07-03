
(* Similar to `Diff.has_diff`, this is used in conjunction
 * with `Hashtbl.iter` so the parameters are to be assumed. *)
let handle_diff cache file cached_mtime =
  let is_stale = Diff.has_diff cache file cached_mtime in
  if is_stale then
    ignore ("this file needs to be parsed and checked again")
  else
    ignore ("do nothing")

(* Checks every file in cache for diffs and handles as needed. *)
let check_all_files cache =
  Hashtbl.iter (fun k v -> handle_diff cache k v) cache

(* This function polls file watching. *)
let rec watch_all_files cache =
  check_all_files cache;
  Unix.sleep 5 (* Wait time between polling checks. *)

(* Creates a new thread that begins to watch all the files in
 * the given cache. Someone else is responsible for filling the
 * cache. *)
let create_watcher cache =
  Thread.create watch_all_files cache
