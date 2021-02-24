module FlowAst = Flow_parser.Ast
module Loc = Flow_parser.Loc

let warn (msg : string) (loc : Loc.t) : unit =
  Printf.printf "\n%s %s ~(line: %d, column: %d)\n"
    (" ⚠" |> Chalk.yellow |> Chalk.bold)
    msg
    loc.start.line
    loc._end.column
