
let report ~msg ~level =
  match level with
  | Level.High ->
    print_endline("\n🚨  FatalError: " ^ msg)
  | Level.Med ->
      print_endline("\n❗ Error: " ^ msg)
  | Level.Low ->
      print_endline("\n🚩  Warning: " ^ msg)
  | Level.SyntaxError ->
    print_endline("\n❗ SyntaxError: " ^ msg)
  | Level.ParseError ->
    print_endline("\n❗ ParseError: " ^ msg)

