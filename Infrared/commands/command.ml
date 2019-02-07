type t = {
  name: string;
  aliases: string list;
  doc: string;
  flags: Flag.t list;
}

let create ~name ~aliases ~doc ~flags =
  { name = name
  ; aliases = aliases
  ; doc = doc
  ; flags = flags
  }
