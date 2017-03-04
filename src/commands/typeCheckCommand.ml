
let check_files = 
  ignore ("check_files")

let spec = 
  let flags = Flag.create_list [
    ("-watch", "Type checks the target files in real time as you edit.")
  ] in
  CommandSpec.create
  ~name:"check"
  ~doc:"Type checks the targetd files and reports back with any errors found."
  ~flags: flags

let exec = check_files;

