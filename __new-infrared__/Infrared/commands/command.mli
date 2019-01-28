type t = {
  name: string;
  doc: string;
  flags: Flag.t list
}

val create : name:string -> doc:string -> flags:Flag.t list -> t
