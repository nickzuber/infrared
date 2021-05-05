let padding_size = 32

let string_of_flags (flags : Flag.t list) : string =
  let strs = List.map (fun flag ->
      let open Flag in
      let spacing = String.make (padding_size - (String.length flag.name)) ' ' in
      Printf.sprintf "\n    %s%s%s"
        flag.name
        spacing
        flag.doc
    ) flags
  in
  String.concat "" strs

let get_help name_and_docs alias_and_docs =
  let title = "Usage: infrared [COMMAND]" in
  let cmds_title = "Valid values for [COMMAND]" in
  let aliases_title = "Valid aliases for [COMMAND]" in
  let cmds = List.fold_left
      (fun msg tuple ->
         let (name, doc, flags) = tuple in
         let flags_strs = string_of_flags flags in
         let spacing = String.make (padding_size - (String.length name)) ' ' in
         let cmsg = Printf.sprintf "  %s%s%s%s" name spacing doc flags_strs in
         msg ^ "\n" ^ cmsg)
      ""
      name_and_docs
  in
  let aliases = List.fold_left
      (fun msg tuple ->
         let (alias, doc) = tuple in
         let spacing = String.make (padding_size - (String.length alias)) ' ' in
         let cmsg = Printf.sprintf "  %s%s%s" alias spacing doc in
         msg ^ "\n" ^ cmsg)
      ""
      alias_and_docs
  in
  Printf.sprintf "%s\n\n%s%s\n\n%s%s\n"
    title
    cmds_title
    cmds
    aliases_title
    aliases


let spec = Command.create
    ~name:"help"
    ~aliases:["-h"; "--help"]
    ~doc:"Print all of the possible usage information"
    ~flags:[]

let exec = get_help

type t =
  { spec: Command.t
  ; exec: (string * string * Flag.t list) list -> (string * string) list -> string
  }

let command : t =
  { spec = spec
  ; exec = exec
  }
