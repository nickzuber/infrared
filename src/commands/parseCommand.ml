
let exec flags = 
  match flags with
  | f :: rest -> print_endline "no flags"
  | _ -> print_endline "there were flags"

let command = {
  Command.
  name = "Parse";
  flags = [];
  exec = exec;
}

