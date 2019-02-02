type valid_command =
  | Help of Help_command.t
  | Version of Version_command.t
  (* | Check of Command.t *)

type any_command =
  | Valid of valid_command
  | Invalid of string

let version_command = Version Version_command.command
let help_command = Help Help_command.command

let any_command_of_string name : any_command =
  match name with
  | "-v"
  | "-version"
  | "--v"
  | "--version"
  | "version" -> Valid version_command
  | "-h"
  | "-help"
  | "--h"
  | "--help"
  | "help" -> Valid help_command
  | _ -> Invalid name

let doc_of_valid_command vc : string =
  match vc with
  | Help h -> h.spec.doc
  | Version v -> v.spec.doc

let name_of_valid_command vc : string =
  match vc with
  | Help h -> h.spec.name
  | Version v -> v.spec.name

module InfraredShell : sig
  val commands : valid_command list
  val report_command_error : string -> unit
  val exec : unit -> unit
end = struct
  let commands = [
    version_command;
    help_command
  ]

  let report_command_error msg =
    Printf.printf "Error: %s\nFor a list of valid commands try `infrared help`\n\n" msg

  let greeting () =
    Printf.printf "%s%s%s\n\n" "Infrared \x1b[1mv"
      (Version_command.exec ())
      "\x1b[0m — Inferred static type checker for JavaScript."

  let exec () =
    let argv = Array.to_list Sys.argv in
    match argv with
    | [] -> report_command_error ""
    | _prgm :: [] -> greeting ()
    | _prgm :: cmd :: _args ->
      let command = any_command_of_string cmd in
      match command with
      | Valid vcmd -> begin
        match vcmd with
        | Version v -> Printf.printf "Infrared \x1b[1mv%s\x1b[0m\n\n" (v.exec ())
        | Help h ->
          let name_and_docs : (string * string) list =
            List.fold_left
              (fun tuples command ->
                ( name_of_valid_command command
                , doc_of_valid_command command
                ) :: tuples)
              []
              commands
          in
          Printf.printf "%s\n" (h.exec name_and_docs)
      end
      | Invalid name ->
        report_command_error
          (Printf.sprintf "Unknown command \"%s\"" name)
end
