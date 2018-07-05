
open Core.Std
open CommandSpec
open Flag

let padding_size = 15

let print_usage commands =
  let open Printf in
  printf "🐢  Usage: infrared [COMMAND]\n\n🐳  Valid values for COMMAND:\n";
  List.iter ~f:(fun cmd ->
      let spacing = String.make (padding_size - (String.length cmd.name)) ' ' in
      let _ = printf "     %s%s%s\n" cmd.name spacing cmd.doc in
      List.iter ~f:(fun flg ->
          let spacing = String.make (padding_size - (String.length flg.flag) - 3) ' ' in
          printf "        %s%s%s\n" flg.flag spacing flg.doc) cmd.flags
    ) commands

let spec = CommandSpec.create
    ~name:"help"
    ~doc:"Prints the usage and other information regarding Infrared."
    ~flags:[]

let exec = print_usage
