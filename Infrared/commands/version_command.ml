let version = "0.0.1"

let get_version () =
  let git_hash_command = "git rev-parse --short HEAD" in
  let version_hash = ref "" in
  let chan = Unix.open_process_in git_hash_command in
  version_hash := (input_line chan);
  let _ = Unix.close_process_in chan in
  version ^ "-" ^ !version_hash

let spec = Command.create
  ~name:"version"
  ~aliases:["-v"; "--version"]
  ~doc:"Print the installed version of Infrared"
  ~flags:[]

let exec = get_version

type t =
  { spec: Command.t
  ; exec: unit -> string
  }

let command : t =
  { spec = spec
  ; exec = exec
  }
