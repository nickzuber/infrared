(* Core Infrared shell for filtering and dispatching commands *)

open Core.Std
open Command 

module InfraredShell : sig
  val commands : Command.t list
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
    List.iter ~f:(fun cmd -> print_endlinef " %s" cmd.doc) commands;
    print_endline (" ✨ 🚀  Infrared v1.0.0")

end

let _ = InfraredShell.main ()

