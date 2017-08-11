
open Loc

(* Reports a simple and general error *)
let report ~msg ~level =
  match level with
  | Level.High ->
    print_endline("\n🆘  \x1b[1;31mFatal Error\x1b[0;39m — " ^ msg)
  | Level.Med ->
    print_endline("\n🚫  \x1b[1;31mError\x1b[0;39m — " ^ msg)
  | Level.Low ->
    print_endline("\n🚸  \x1b[1;33mWarning\x1b[0;39m — " ^ msg)
  | Level.SyntaxError ->
    print_endline("\n🅾️  \x1b[1;31mSyntax Error\x1b[0;39m — " ^ msg)
  | Level.ParseError ->
    print_endline("\n⭕️  \x1b[1;31mParsing Error\x1b[0;39m — " ^ msg)

(* Locates the offending area in the given source file, converts to a string and returns it. 
 * This string is generally thrown somewhere else. *)
let exposed_error ~source ~loc ~msg =
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
    Error was found in \x1b[4;33m%s\x1b[0;39m at %d:%d\n\
    \x1b[35m❯\x1b[39m \x1b[3m%s\x1b[0m\n\n\
    \x1b[90m%4d │ %s\n\
    \x1b[90m%4d │ %s\n\
    \x1b[90m%4d │\x1b[39m %s\n\
   \x1b[90m     │\x1b[31m%s\x1b[39m\n\
    \x1b[90m%4d │ %s \x1b[39m\n\
    \x1b[90m%4d │ %s \x1b[39m\n"
    source
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
