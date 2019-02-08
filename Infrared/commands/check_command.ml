let type_check args =
  match args with
  | [] -> ()
  | arg :: [] ->
    let paths = InfraredUtils.Fs.files_from_path arg in
    (* @TODO *)
    let _ = List.map (fun arg -> Printf.printf "%s\n" arg) paths
    in ()
  | args ->
    let paths = args
      |> List.map InfraredUtils.Fs.files_from_path
      |> List.flatten
    in
    (* @TODO *)
    let _ = List.map (fun arg -> Printf.printf "%s\n" arg) paths
    in ()

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
