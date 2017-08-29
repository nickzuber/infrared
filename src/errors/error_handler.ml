
open Loc

let use_inline_error_marking = true

(* Reports a simple and general error *)
let report ~msg ~level =
  match level with
  | Level.High ->
    print_endline("\n\x1b[1;97;41m FATAL ERROR \x1b[0;39;0m " ^ msg)
  | Level.Med ->
    print_endline("\n\x1b[1;97;101m ERROR \x1b[0;39;0m " ^ msg)
  | Level.Low ->
    print_endline("\n\x1b[1;30;43m WARNING \x1b[0;39;0m " ^ msg)
  | Level.SyntaxError ->
    print_endline("\n\x1b[1;97;101m SYNTAX ERROR \x1b[0;39;0m " ^ msg)
  | Level.ParseError ->
    print_endline("\n\x1b[1;97;101m PARSING ERROR \x1b[0;39;0m " ^ msg)

(* Locates the offending area in the given source file, converts to a string and returns it. 
 * This string is generally thrown somewhere else. *)
let rec exposed_error ~source ~loc ~msg =
  if use_inline_error_marking <> true then
    expose_error_fallback ~source:source ~loc:loc ~msg:msg
  else
    exposed_error_with_markings ~source:source ~loc:loc ~msg:msg

(* Error message with physical markings to indicate an error.
 * const foo = var
 *             ^^^
 *)
and expose_error_fallback ~source ~loc ~msg =
  let source_path, source_file = Utils.depath source in
  let most_upper_line = ref "" in
  let upper_line = ref "" in
  let offending_line = ref "" in
  let lower_line = ref "" in
  let most_lower_line = ref "" in
  let spacing = String.make (loc.column) ' ' in
  let arrow = String.make (loc.length) '^' in
  let lines = Batteries.File.lines_of source in
  let _ = Batteries.Enum.fold 
    (fun cur_line line -> 
      if cur_line = (loc.line - 2) then most_upper_line := line else ();
      if cur_line = (loc.line - 1) then upper_line := line else ();
      if cur_line = loc.line then offending_line := line else ();
      if cur_line = (loc.line + 1) then lower_line := line else ();
      if cur_line = (loc.line + 2) then most_lower_line := line else ();
      cur_line + 1
    ) 1 lines
  in Printf.sprintf "\
    %s\x1b[1m%s\x1b[0m \x1b[90m(%d:%d)\x1b[39m\n\n\
    \x1b[31m  ● \x1b[39m%s\n\n\
    \x1b[90m%4d │ %s\n\
    \x1b[90m%4d │ %s\n\
    \x1b[39m%4d │ %s\n\
   \x1b[90m     │\x1b[31m%s\x1b[39m\n\
    \x1b[90m%4d │ %s \x1b[39m\n\
    \x1b[90m%4d │ %s \x1b[39m\n"
    source_file
    source_path
    loc.line
    loc.column
    msg
    (loc.line - 2)
    !most_upper_line
    (loc.line - 1)
    !upper_line
    loc.line
    !offending_line
    (spacing ^ arrow)
    (loc.line + 1)
    !lower_line
    (loc.line + 2)
    !most_lower_line

(* Error message with colored markings to indicate an error.
 * const foo = var
 *              │
 *              └── `var` is red here
 *)
and exposed_error_with_markings ~source ~loc ~msg =
  let source_path, source_file = Utils.depath source in
  let most_upper_line = ref "" in
  let upper_line = ref "" in
  let offending_line = ref "" in
  let lower_line = ref "" in
  let most_lower_line = ref "" in
  let lines = Batteries.File.lines_of source in
  let _ = Batteries.Enum.fold 
    (fun cur_line line -> 
      if cur_line = (loc.line - 2) then most_upper_line := line else ();
      if cur_line = (loc.line - 1) then upper_line := line else ();
      if cur_line = loc.line then offending_line := line else ();
      if cur_line = (loc.line + 1) then lower_line := line else ();
      if cur_line = (loc.line + 2) then most_lower_line := line else ();
      cur_line + 1
    ) 1 lines
  in Printf.sprintf "\
    %s\x1b[1m%s\x1b[0m \x1b[90m(%d:%d)\x1b[39m\n\n\
    \x1b[31m  ● \x1b[39m%s\n\n\
    \x1b[90m%4d │ %s\n\
    \x1b[90m%4d │ %s\n\
    \x1b[39m%4d │ %s\x1b[41m%s\x1b[49m%s\n\
    \x1b[90m%4d │ %s \x1b[39m\n\
    \x1b[90m%4d │ %s \x1b[39m\n"
    source_file
    source_path
    loc.line
    loc.column
    msg
    (loc.line - 2)
    !most_upper_line
    (loc.line - 1)
    !upper_line
    loc.line
    (String.sub !offending_line 0 (loc.column - 1))
    (String.sub !offending_line (loc.column - 1) loc.length)
    (String.sub !offending_line (loc.column - 1 + loc.length) ((String.length !offending_line) - (loc.column + loc.length) + 1))
    (loc.line + 1)
    !lower_line
    (loc.line + 2)
    !most_lower_line
