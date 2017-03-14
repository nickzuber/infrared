
{
module Token = struct

  (* These keywords are organized alphabetically rather than by relevant
   * association, and separated by their specifications. Keep that in mind when
   * looking for particular keywords to add or change.
   * https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Lexical_grammar *)
  type t = 
    (* Standard *)
    | Expression of t list
    | Variable of t_variable
    | Number
    | Bool
    | String
    | Comment
    | Break
    | Case
    | Catch
    | Continue
    | Debugger
    | Default
    | Delete
    | Do
    | Else
    | Export
    | Extends
    | Finally
    | For
    | Function
    | If
    | Import
    | In
    | Instanceof
    | New
    | Return
    | Super
    | Switch
    | This
    | Throw
    | Try
    | Typeof
    | Void
    | While
    | With
    | Yield
    (* ES6 *)
    | Class
    | Implements
    | Spread
    | TemplateString
    | Rest
    (* ES7 *)
    | Async
    | Await
    (* Miscellaneous & FutureReserved *)
    | Enum
    | Interface
    | Package
    | Private
    | Protected
    | Public
    | Static
    | Eof

  type t_variable = 
    (* Standard *)
    | Var
    (* ES6 *)
    | Let
    | Const

end
open Token
}

rule token env = parse
  | '\n'

