
type t = {
  name: string;
  doc: string;
  flags: Flag.t list;
  exec: args:string list -> flags:Flag.t list -> (Ast.t ,string) result list;
}

