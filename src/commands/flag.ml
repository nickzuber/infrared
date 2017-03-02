
type t = {
  flag: string;
  doc: string;
}

let create ~flag ~doc = {
  flag = flag;
  doc = doc;
}

