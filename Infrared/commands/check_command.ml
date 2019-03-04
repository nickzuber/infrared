open InfraredUtils

let check_file (file : string) : unit =
  let source = Fs.read_file file in
  let ast = InfraredParser.Parser.parse_source source in
  Printf.printf "\x1b[1;4m%s:\x1b[0;0m\n%s\n\n\n"
    file
    (InfraredParser.Printer.string_of_ast ast)

let check_files (files : string list) : unit =
  let _ = List.map (fun file -> check_file file) files
  in ()

let type_check args =
  match args with
  | [] -> ()
  | arg :: [] ->
    let paths = Fs.files_from_path arg in
    check_files paths
  | args ->
    let paths =
      args
      |> List.map Fs.files_from_path
      |> List.flatten
    in
    check_files paths

let spec = Command.create
    ~name:"check"
    ~aliases:["ch"]
    ~doc:"Type check the given JavaScript files"
    ~flags:[]

let exec = type_check

type t =
  { spec: Command.t
  ; exec: string list -> unit
  }

let command : t =
  { spec = spec
  ; exec = exec
  }
