
let extract_files args =
  []

(* Check the first character to determine is flag or not. *)
let is_flag arg =
  let first = String.get arg 0 in
  if first = '-' then true else false

(* Separate arguments and flags and produce a tuple. *)
let sanitize_args args =
  List.partition is_flag args

