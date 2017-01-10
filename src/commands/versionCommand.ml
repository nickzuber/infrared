
open Core.Std
open CommandSpec

let print_version () =
  Printf.printf "🎺  Currenting running Infrared v%s\n" InfraredConfig.version

let command = CommandSpec.create_command
  ~name:"version"
  ~doc:"Prints the current version."
  ~flags:[]

let exec = print_version
