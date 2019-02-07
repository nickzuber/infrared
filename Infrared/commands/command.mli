type t = {
  name: string;
  aliases: string list;
  doc: string;
  flags: Flag.t list
}

val create : name:string -> aliases:string list -> doc:string -> flags:Flag.t list -> t
