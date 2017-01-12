(* Core Infrared shell for filtering and dispatching commands *)

open CommandSpec

module InfraredShell : sig
  val commands : CommandSpec.t list
  val main : unit -> unit
  val greeting : unit -> unit
  val failure : string -> unit
end = struct
  let commands = [
    HelpCommand.spec;
    ParseCommand.spec;
    VersionCommand.spec;
(*    
    TypeCheckCommand.spec;
*)  
  ]

  let failure msg = 
    Printf.printf "😬  Well this is awkward, %s\n" msg

  let greeting () = 
    Printf.printf "%s%s\n\n" "✨  🚀  Infrared — " InfraredConfig.version

  let main () = 
    let argv = Array.to_list Sys.argv in
    match argv with
    | [] -> failure "no args found whatsoever, shouldn't ever see this"
    | prgm :: [] -> HelpCommand.exec commands
    | prgm :: cmd :: flags -> 
        try 
          let command = List.find (fun command -> 
            command.name = cmd) commands in
          match command with
          | cmd when cmd.name = HelpCommand.spec.name -> HelpCommand.exec commands
          | _ -> raise Not_found
        with Not_found ->
          failure "you've entered an invalid command.\n";
          HelpCommand.exec commands
end

let _ = 
  (* InfraredShell.greeting (); *)
  InfraredShell.main ()

