
{
module Token = struct
  type t = 
    (* Standard *)
    | NUMBER
    | BOOL
    | STRING
    | EXPR of t list
    | EOF



end
open Token
}

rule token env = parse
  | '\n'
