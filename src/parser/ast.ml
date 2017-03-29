
module Token = Lexer.Token
module Lex_env = Lexer.Lex_env

(* Dope trick for defining recursive modules
 * https://blogs.janestreet.com/a-trick-recursive-modules-from-recursive-signatures/ *)

module rec Loc : sig
  type t = { line: int }
end = Loc

and Program : sig
  type t = (string * Type.t list)
end = Program

and Identifier : sig
  type t = (string * Loc.t)
end = Identifier

and Expression : sig
  type t
end = Expression

and Object : sig
  type t
end = Object

and Literal : sig
  module Number : sig
    type t = (float * Loc.t)
  end

  module Boolean : sig
    type t = (bool * Loc.t)
  end

  module String : sig
    type t = (string * Loc.t)
  end
end = Literal

and VariableDeclaration : sig
  type t = {
    declarations: t' list;
    kind: var_t;
  }

  and t' =
    | Identifier of Identifier.t
    | Function of Function.t
    | Expression of Expression.t
    | Literal of Literal.t
    | Object of Object.t

  and var_t =
    | Var
    | Let
    | Const
end = VariableDeclaration

