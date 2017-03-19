
{
module Token = struct

  (* These keywords are organized alphabetically rather than by relevant
   * association, and separated by their specifications. Keep that in mind when
   * looking for particular keywords to add or change.
   * https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Lexical_grammar *)
  type t = 
    (* Standard *)
    | Expression of t list
    | Variable of var_t
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

  type var_t = 
    (* Standard *)
    | Var
    (* ES6 *)
    | Let
    | Const

  type state_t = 
    | REGULAR
    | PAREN_OPEN
    | PAREN_CLOSE
    | BRACKET_OPEN
    | BRACKET_CLOSE

  type env_t = {
    mutable state: state_t list;
    mutable expr: t list;
    mutable body_builder: t list list;
    mutable ast: t list;
  }

end
open Token
}

rule token env = parse
  | '\n'

