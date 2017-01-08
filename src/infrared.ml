(* Core Infrared shell for filtering and dispatching commands *)

module InfraredShell : sig
  val commands : Command.t list
  val main : unit -> unit
end = struct
  let commands = [
    ParseCommand.command;
    TypeCheckCommand.command;
    HelpCommand.command;
    VersionCommand.command;
  ]

  let main () = 
    print_endline ("collect cli args and defer to respective command to handle")

end

