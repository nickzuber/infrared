let padding_size = 10

let get_help name_and_doc =
  let title = "Usage: infrared [COMMAND]" in
  let cmds_title = "Valid values for [COMMAND]" in
  let cmds = List.fold_left
    (fun msg tuple ->
      let (name, doc) = tuple in
      let spacing = String.make (padding_size - (String.length name)) ' ' in
      let cmsg = Printf.sprintf "  %s%s%s" name spacing doc in
      msg ^ "\n" ^ cmsg)
    ""
    name_and_doc
  in
  Printf.sprintf "%s\n\n%s%s\n"
    title
    cmds_title
    cmds


let spec = Command.create
  ~name:"help"
  ~aliases:["-h"; "--help"]
  ~doc:"Print all of the possible usage information"
  ~flags:[]

let exec = get_help

type t =
  { spec: Command.t
  ; exec: (string * string) list -> string
  }

let command : t =
  { spec = spec
  ; exec = exec
  }
