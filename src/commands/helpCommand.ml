
open Core.Std

let get_commands = [
  ParseCommand.command;
]

let print_usage ~args ~flags =
  let open Printf in
  let commands = get_commands in
  printf "
    Usage: infrared [COMMAND]\n\n
    Valid options for COMMAND:\n";
  List.iter ~f:(fun cmd -> 
    printf "  %s\t\t%s" cmd.name cmd.doc) commands

let command = {
  Command.
  name = "help";
  doc = "Prints the usage and other information regarding Infrared.";
  flags = [];
  exec = print_usage;
}

