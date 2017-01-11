(* Core Infrared shell for filtering and dispatching commands *)

open Core.Std
open CommandSpec

module InfraredShell : sig
  val commands : CommandSpec.t list
  val main : unit -> unit
  val greeting : unit -> unit
end = struct
  let commands = [
    ParseCommand.command;
    HelpCommand.command;
    VersionCommand.command;
(*    
    TypeCheckCommand.command;
*)  
  ]

  let greeting () = 
    Printf.printf "%s%s"
      "✨  🚀  Infrared v1.0.1 — "
      "Fast light weight inferred static type checker in real time for JavaScript.\n\n"

  let main () = 
    HelpCommand.exec commands
end

let _ = 
  InfraredShell.greeting ();
  InfraredShell.main ();

