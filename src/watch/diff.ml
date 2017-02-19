
let get_mtime file =
  Unix.((stat file).st_mtime)

let cache = Hashtbl.create 50

let check_diff file ~cache = 
  let file_mtime = get_mtime file in
  try
    let cached_mtime = Hashtbl.find cache file in
    if cached_mtime <> file_mtime then
      Hashtbl.replace cache file file_mtime;
      (* This file has been updated and needs to be rechecked *)
    else 
      (* This file has not been changed *)
  with
    Not_found -> 
      Hashtbl.add cache file file_mtime;
      (* This file has not been tracked or found before, so we obviously
       * need to be checked *)



