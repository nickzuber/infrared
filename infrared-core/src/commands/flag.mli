type t = { flag : string; doc : string; }
val create : flag:string -> doc:string -> t
val create_list : (string * string) list -> t list
