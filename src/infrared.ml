(* Core Infrared shell for filtering and dispatching commands *)

open Core.Std
open CommandSpec

module InfraredShell : sig
  val commands : CommandSpec.t list
  val main : unit -> unit
end = struct
  let commands = [
    ParseCommand.command;
    HelpCommand.command;
(*    
    TypeCheckCommand.command;
    VersionCommand.command;
*)  
  ]

  let main () = 
    print_endline ("✨  🚀  Infrared v0.0.1 — " ^
    "Fast light weight inferred static type checker in real time for JavaScript.\n");
    HelpCommand.exec commands

end

let _ = InfraredShell.main ()

