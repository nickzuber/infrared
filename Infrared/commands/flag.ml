type t = {
  name: string;
  doc: string;
}

let create ~name ~doc =
  { name = name
  ; doc = doc
  }
