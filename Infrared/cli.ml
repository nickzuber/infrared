type valid_command =
  | Help of Help_command.t
  | Version of Version_command.t
  | Check of Check_command.t
  | Parse of Parse_command.t

type any_command =
  | Valid of valid_command
  | Invalid of string

let version_command = Version Version_command.command
let help_command = Help Help_command.command
let check_command = Check Check_command.command
let parse_command = Parse Parse_command.command

let any_command_of_string name : any_command =
  match name with
  | "-v"
  | "--version"
  | "version" -> Valid version_command
  | "-h"
  | "--help"
  | "help" -> Valid help_command
  | "ch"
  | "check" -> Valid check_command
  | "pp"
  | "parse" -> Valid parse_command
  | _ -> Invalid name

let doc_of_valid_command vc : string =
  match vc with
  | Help h -> h.spec.doc
  | Version v -> v.spec.doc
  | Check c -> c.spec.doc
  | Parse p -> p.spec.doc

let name_of_valid_command vc : string =
  match vc with
  | Help h -> h.spec.name
  | Version v -> v.spec.name
  | Check c -> c.spec.name
  | Parse p -> p.spec.name

let aliases_of_valid_command vc : string list =
  match vc with
  | Help h -> h.spec.aliases
  | Version v -> v.spec.aliases
  | Check c -> c.spec.aliases
  | Parse p -> p.spec.aliases

module InfraredShell : sig
  val commands : valid_command list
  val report_command_error : string -> unit
  val exec : unit -> unit
end = struct
  let commands = [
    version_command;
    help_command;
    check_command;
    parse_command;
  ]

  let report_command_error msg =
    Printf.printf "Error: %s\nFor a list of valid commands try `infrared help`\n\n" msg

  let greeting () =
    Printf.printf "%s%s%s\n\n"
      "Running \x1b[1minfrared\x1b[0m v"
      (Version_command.exec ())
      " — Inferred static type checker for JavaScript."

  let exec () =
    let argv = Array.to_list Sys.argv in
    match argv with
    | [] -> report_command_error ""
    | _prgm :: [] -> greeting ()
    | _prgm :: cmd :: args ->
      let command = any_command_of_string cmd in
      match command with
      | Valid vcmd -> begin
          match vcmd with
          | Check c -> c.exec args
          | Parse p -> p.exec args
          | Version v -> Printf.printf "v%s\n\n" (v.exec ())
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
            let aliases_and_docs : (string * string) list =
              List.fold_left
                (fun tuples command ->
                   let command_name = name_of_valid_command command in
                   let aliases = aliases_of_valid_command command in
                   let aliases_and_docs = List.fold_left
                       (fun tuples' alias ->
                          ( alias
                          , "Alias for " ^ command_name
                          ) :: tuples')
                       []
                       aliases
                   in
                   aliases_and_docs @ tuples)
                []
                commands
            in
            let all_tuples = name_and_docs @ aliases_and_docs in
            Printf.printf "%s\n" (h.exec all_tuples)
        end
      | Invalid name ->
        report_command_error
          (Printf.sprintf "Unknown command \"%s\"" name)
end
