type t = {
  name: string;
  doc: string;
  flags: Flag.t list;
}

let create ~name ~doc ~flags =
  { name = name
  ; doc = doc
  ; flags = flags
  }
