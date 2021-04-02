open InfraredParser.Ast

let assign_types ~(file : string) ~(program : program) =
  let _ = file in
  program
  |> Remove_assignify.transform
  |> Uniquify.transform
  |> Typify.transform
