open CommandSpec
open Ast

module InfraredShell : sig
  val commands : CommandSpec.t list
  val main : unit -> unit
  val greeting : unit -> unit
  val reportCommandError : string -> unit
end = struct
  let commands = [
    HelpCommand.spec;
    TokenizeCommand.spec;
    ParseCommand.spec;
    CheckCachedCommand.spec;
    TypeCheckCommand.spec;
    VersionCommand.spec;
  ]

  let reportCommandError msg =
    Printf.printf "\n😬  Well this is awkward, %s\n" msg

  let greeting () =
    Printf.printf "%s%s%s\n\n" "✨  🚀  Infrared v"
      InfraredConfig.version
      " — Fast light weight inferred static type checker in real time for JavaScript."

  let main () =
    let argv = Array.to_list Sys.argv in
    match argv with
    | [] -> reportCommandError "no args found whatsoever, shouldn't ever see this"
    | prgm :: [] ->
      greeting ();
      HelpCommand.exec commands
    | prgm :: cmd :: args ->
      try
        let command = List.find (fun command ->
            command.name = cmd) commands in
        (* Cannot have arbitrary function as a type param, therefore we need
         * to try and match commands like this because we cannot store custom
         * exec functions to the commands themselves, but rather to their
         * modules. I'm sure there's a "right" way to do this so keep thinking. *)
        let parsedArgs = Fs.sanitize_args args in
        let (flags', args') = parsedArgs in
        match command.name with
        | cmd when HelpCommand.spec.name = cmd -> HelpCommand.exec commands
        | cmd when TokenizeCommand.spec.name = cmd ->
          (match args' with
           | [] -> reportCommandError "no arguments given for tokenizing. \
                                       Did you forget to include a file name?"
           | arg :: [] ->
             Parser.print_tokens (TokenizeCommand.exec ~flags:flags' ~args:[arg])
           | _ ->
             (* temp *)
             Printf.printf "\nflags found\n";
             Core.Std.List.iter ~f:(fun file -> Printf.printf "%s\n" file) flags';
             Printf.printf "\nPRINTING FILES FOUND: \n";
             Parser.print_tokens (TokenizeCommand.exec ~flags:flags' ~args:args'))
        | cmd when VersionCommand.spec.name = cmd -> VersionCommand.exec ()
        | cmd when ParseCommand.spec.name = cmd ->
          (match args' with
           | [] -> reportCommandError "no arguments given for parsing. \
                                       Did you forget to include a file name?"
           | arg :: [] ->
             Parser.print_ast (ParseCommand.exec ~flags:flags' ~args:[arg])
           | _ ->
             (* temp *)
             Printf.printf "\nflags found\n";
             Core.Std.List.iter ~f:(fun file -> Printf.printf "%s\n" file) flags';
             Printf.printf "\nParsing files found: \n";
             Parser.print_ast (ParseCommand.exec ~flags:flags' ~args:args'))
        | cmd when CheckCachedCommand.spec.name = cmd ->
          (match args' with
           | [] -> reportCommandError "no arguments given for parsing. \
                                       Did you forget to include a file name?"
           | arg :: [] ->
             CheckCachedCommand.exec ~flags:flags' ~args:[arg]
           | _ ->
             (* temp *)
             (* Core.Std.List.iter ~f:(fun file -> Printf.printf "%s\n" file) flags'; *)
             CheckCachedCommand.exec ~flags:flags' ~args:args')
        | cmd when TypeCheckCommand.spec.name = cmd ->
          (match args' with
           | [] -> reportCommandError "no arguments given for parsing. \
                                       Did you forget to include a file name?"
           | arg :: [] ->
             TypeCheckCommand.exec ~flags:flags' ~args:[arg]
           | _ ->
             (* temp *)
             (* Core.Std.List.iter ~f:(fun file -> Printf.printf "%s\n" file) flags'; *)
             TypeCheckCommand.exec ~flags:flags' ~args:args')
        | _ -> raise Not_found
      with Not_found ->
        reportCommandError ("you've entered an invalid command: \n\t" ^ cmd ^ "\n");
        HelpCommand.exec commands
end

let _ = Token_parser.(
    try
      InfraredShell.main ()
    with
    | _ as e ->
      let string_of_error = Printexc.to_string e in
      let message = Printf.sprintf "Uncaught error was thrown: \n\n\t%s\n" string_of_error in
      (Error_handler.report ~source:"" ~msg:message ~level:Level.High))
