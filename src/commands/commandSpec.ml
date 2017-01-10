
type t = {
  name: string;
  doc: string;
  flags: Flag.t list;
}

let create_command ~name ~doc ~flags = {
  name = name;
  doc = doc;
  flags = flags;
}

