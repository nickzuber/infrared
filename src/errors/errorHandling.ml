
let report ~msg ~level =
  match level with
  | Level.High ->
    print_endline("🚨  Serious error: " ^ msg)
  | Level.Med ->
      print_endline("❗ Oh boy.. ")
  | Level.Low ->
      print_endline("⚠️  Ye be warned, " ^ msg)

