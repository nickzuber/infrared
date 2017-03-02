
type t = {
  flag: string;
  doc: string;
}

let create ~name ~doc ~flags = {
  name = name;
  doc = doc;
  flags = flags;
}

