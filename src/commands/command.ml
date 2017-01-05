
(* The structure of a generic command *)
type t = {
  name: string;
  flags: Flag.t list;
  exec: Flag.t list -> unit;
}

