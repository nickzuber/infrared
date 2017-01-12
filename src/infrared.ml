(* Core Infrared shell for filtering and dispatching commands *)

open Core.Std
open CommandSpec

module InfraredShell : sig
  val commands : CommandSpec.t list
  val main : unit -> unit
  val greeting : unit -> unit
  val failure : string -> unit
end = struct
  let commands = [
    ParseCommand.command;
    HelpCommand.command;
    VersionCommand.command;
(*    
    TypeCheckCommand.command;
*)  
  ]

  let failure msg = 
    Printf.printf "🔥  Whoops, %s" msg

  let greeting () = 
    Printf.printf "%s%s"
      "✨  🚀  Infrared v1.0.1 — "
      "Fast light weight inferred static type checker in real time for JavaScript.\n\n"

  let main () = 
    let argv = Array.to_list Sys.argv in
    let command = match argv with
    | [] -> failure "no args found whatsoever, shouldn't ever see this"
    | prgm :: [] -> HelpCommand.exec commands
    | prgm :: cmd :: rest -> VersionCommand.exec ()
    in command
end

let _ = 
  InfraredShell.greeting ();
  InfraredShell.main ();

