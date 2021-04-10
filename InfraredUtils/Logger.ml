module FlowAst = Flow_parser.Ast
module Loc = Flow_parser.Loc

let warn (msg : string) (loc : Loc.t) : unit =
  Printf.printf "%s %s %s\n"
    (" ⚠" |> Chalk.yellow |> Chalk.bold)
    ((Printf.sprintf "(%d:%d)" loc.start.line loc._end.column) |> Chalk.gray)
    msg

let error (msg : string) (loc : Loc.t) : unit =
  Printf.printf "%s %s %s\n"
    (" ⃠ " |> Chalk.red |> Chalk.bold)
    ((Printf.sprintf "(%d:%d)" loc.start.line loc._end.column) |> Chalk.gray)
    msg

let error_no_loc (msg : string) : unit =
  Printf.printf "%s (?:?) %s\n"
    (" ⃠ " |> Chalk.red |> Chalk.bold)
    msg
