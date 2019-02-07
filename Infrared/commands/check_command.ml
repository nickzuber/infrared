let type_check () =
  ()

let spec = Command.create
  ~name:"check"
  ~aliases:["ch"]
  ~doc:"Type check the given JavaScript files"
  ~flags:[]

let exec = type_check

type t =
  { spec: Command.t
  ; exec: unit -> unit
  }

let command : t =
  { spec = spec
  ; exec = exec
  }
