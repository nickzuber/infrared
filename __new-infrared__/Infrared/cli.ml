module InfraredShell : sig
  val commands : Command.t list
  val exec : unit -> unit
end = struct
  let commands = []
  let exec () =
    let _ = Printf.printf "CLI EXEC\n" in
    ()
end
