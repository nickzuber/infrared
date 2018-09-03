
open Loc

let use_inline_error_marking = false

(* Reports a simple and general error *)
let report ~msg ~level =
  match level with
  | Level.High -> Printf.eprintf "\n\x1b[31mFatal error.\x1b[39m\n\n%s" msg
  | Level.Med -> Printf.eprintf "\n\x1b[31mError.\x1b[39m\n\n%s" msg
  | Level.Low -> Printf.eprintf "\n\x1b[33mWarning.\x1b[39m\n\n%s" msg
  | Level.SyntaxError -> Printf.eprintf "\n\x1b[33mSyntax error.\x1b[39m\n\n%s" msg
  | Level.ParseError -> Printf.eprintf "\n\x1b[31mParsing error.\x1b[39m\n\n%s" msg
  | Level.UnknownError -> Printf.eprintf "\n\x1b[33mUnknown error.\x1b[39m\n\n%s" msg

(* Returns a tuple of line, column, and length from a loc object. *)
let loc_to_pos loc = (loc.line, loc.column, loc.length)

(* Locates the offending area in the given source file, converts to a string and returns it.
 * This string is generally thrown somewhere else.
 * Consumes a location object. *)
let rec exposed_error_loc ~source ~loc ~reason ~msg =
  let (loc_line, loc_column, loc_length) = loc_to_pos loc in
  if use_inline_error_marking <> true then
    expose_error_fallback ~source:source ~loc_line:loc_line ~loc_column:loc_column ~loc_length:loc_length ~reason:msg
  else
    exposed_error_with_markings ~source:source ~loc_line:loc_line ~loc_column:loc_column ~loc_length:loc_length ~reason:reason

(* Locates the offending area in the given source file, converts to a string and returns it.
 * This string is generally thrown somewhere else. *)
and exposed_error ~source ~loc_line:loc_line ~loc_column:loc_column ~loc_length:loc_length  ~reason ~msg =
  if use_inline_error_marking <> true then
    expose_error_fallback ~source:source ~loc_line:loc_line ~loc_column:loc_column ~loc_length:loc_length ~reason:msg
  else
    exposed_error_with_markings ~source:source ~loc_line:loc_line ~loc_column:loc_column ~loc_length:loc_length ~reason:reason

(* Error message with physical markings to indicate an error.
 * const foo = var
 *             ^^^
*)
and expose_error_fallback ~source ~loc_line ~loc_column ~loc_length ~reason =
  (* let source_path, source_file = Utils.depath source in *)
  let source_file = "\x1b[90m" ^ source ^ "\x1b[39m" in
  let most_upper_line = ref "" in
  let upper_line = ref "" in
  let offending_line = ref "" in
  let lower_line = ref "" in
  let most_lower_line = ref "" in
  let spacing = String.make (loc_column) ' ' in
  let arrow = String.make (loc_length) '^' in
  let lines = Batteries.File.lines_of source in
  let _ = Batteries.Enum.fold
      (fun cur_line line ->
         if cur_line = (loc_line - 2) then most_upper_line := line else ();
         if cur_line = (loc_line - 1) then upper_line := line else ();
         if cur_line = loc_line then offending_line := line else ();
         if cur_line = (loc_line + 1) then lower_line := line else ();
         if cur_line = (loc_line + 2) then most_lower_line := line else ();
         cur_line + 1
      ) 1 lines
  in Printf.sprintf "\
    %s\n\n\
    \x1b[31m  ● \x1b[39m%s\n\n\
    \x1b[90m%4d | %s\n\
    \x1b[39m%4d | %s\n\
   \x1b[90m     |\x1b[1;31m%s\x1b[0;39m\n\
    \x1b[90m%4d | %s \x1b[39m\n"
    source_file
    reason
    (loc_line - 1)
    !upper_line
    loc_line
    !offending_line
    (spacing ^ arrow)
    (loc_line + 1)
    !lower_line

(* Error message with colored markings to indicate an error.
 * const foo = var
 *              │
 *              └── `var` is red here
*)
and exposed_error_with_markings ~source ~loc_line ~loc_column ~loc_length ~reason =
  let source_path, source_file = Utils.depath source in
  let most_upper_line = ref "" in
  let upper_line = ref "" in
  let offending_line = ref "" in
  let lower_line = ref "" in
  let most_lower_line = ref "" in
  let lines = Batteries.File.lines_of source in
  let _ = Batteries.Enum.fold
      (fun cur_line line ->
         if cur_line = (loc_line - 2) then most_upper_line := line else ();
         if cur_line = (loc_line - 1) then upper_line := line else ();
         if cur_line = loc_line then offending_line := line else ();
         if cur_line = (loc_line + 1) then lower_line := line else ();
         if cur_line = (loc_line + 2) then most_lower_line := line else ();
         cur_line + 1
      ) 1 lines
  in Printf.sprintf "\
    %s\x1b[1m%s\x1b[0m \x1b[90m(%d:%d)\x1b[39m\n\n\
    \x1b[31m  ● \x1b[39m%s\n\n\
    \x1b[90m%4d | %s\n\
    \x1b[90m%4d | %s\n\
    \x1b[39m%4d | %s\x1b[41m%s\x1b[49m%s\n\
    \x1b[90m%4d | %s \x1b[39m\n\
    \x1b[90m%4d | %s \x1b[39m\n"
    source_file
    source_path
    loc_line
    loc_column
    reason
    (loc_line - 2)
    !most_upper_line
    (loc_line - 1)
    !upper_line
    loc_line
    (String.sub !offending_line 0 (loc_column - 1))
    (String.sub !offending_line (loc_column - 1) loc_length)
    (String.sub !offending_line (loc_column - 1 + loc_length) ((String.length !offending_line) - (loc_column + loc_length) + 1))
    (loc_line + 1)
    !lower_line
    (loc_line + 2)
    !most_lower_line
