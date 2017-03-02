
let check_files = 
  ignore ("check_files")

let spec = 
  let flags = Flag.create_list [
    ("-watch", "Type checks the target file(s) in real time as you edit.")
  ] in
  CommandSpec.create
  ~name:"check"
  ~doc:""
  ~flags: flags

let exec = check_files;

