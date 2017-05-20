
let report ~msg ~level =
  match level with
  | Level.High ->
    print_endline("🚨  FatalError: " ^ msg)
  | Level.Med ->
      print_endline("❗ Error: " ^ msg)
  | Level.Low ->
      print_endline("🚩 Warning: " ^ msg)
  | Level.SyntaxError ->
    print_endline("❗ SyntaxError: " ^ msg)
  | Level.ParseError ->
    print_endline("❗ ParseError: " ^ msg)

