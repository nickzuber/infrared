
module Token = Lexer.Token
module Lex_env = Lexer.Lex_env

(* Dope trick for defining recursive modules
 * https://blogs.janestreet.com/a-trick-recursive-modules-from-recursive-signatures/ *)

(*
 * NOTE: This AST implementation is based off of the Shift AST specification
 * described here: 
 * http://shift-ast.org/
 * *)

module rec Loc : sig
  type t = { line: int; }
end = Loc

and VariableDeclarationKind : sig
  type t = 
    | Var
    | Let
    | Const 
end = VariableDeclarationKind

and CompoundAssignmentOperator : sig 
  type t = 
    | Plus                (*    +=    *)
    | Minus               (*    -=    *)
    | Mult                (*    *=    *)
    | Div                 (*    /=    *)
    | Mod                 (*    %=    *)
    | Pow                 (*    **=   *)
    | LeftShift           (*    <<=   *)
    | RightShift          (*    >>=   *)
    | RightShiftUnsigned  (*    >>>=  *)
    | Or                  (*    |=    *)
    | XOr                 (*    ^=    *)
    | And                 (*    &=    *)
end = CompoundAssignmentOperator

and BinaryOperator : sig 
  type t = 
    | Equal
    | NotEqual
    | StrictEqual 
    | StrictNotEqual
    | LessThan
    | LessThanEqual
    | GreaterThan
    | GreaterThanEqual
    | In
    | InstanceOf
    | LeftShift
    | RightShift
    | RightShiftUnsigned
    | Plus
    | Minus
    | Mult
    | Div
    | Mod
    | Pow
    | Comma
    | LogicalOr
    | LogicalAnd
    | Or
    | Xor
    | And
end = BinaryOperator


and ArrayAssignmentTarget : sig
  type t
end = ArrayAssignmentTarget

(*
module rec Loc : sig
  type t = {
    line: int
  }
end = Loc

and Program : sig
  type t = (string * Statement.t list)
end = Program

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

  type t = 
    | Number of Number.t
    | Boolean of Boolean.t
    | String of String.t
    | Null
end = Literal

and Statement : sig
  module Identifier : sig
    type t = (string * Loc.t)
  end

  module Expression : sig
    module BlockExpression : sig
      type t = (Statement.t list * Loc.t)
    end

    module StandardExpression : sig
      type t
    end

    module CallExpression : sig
      type t' = 
        | Identifier of Identifier.t
        | C

      type t = {
        arguments: Statement.t list;
        callee: Identifier.t;
      }
    end

    type t = 
      | BlockExpression of BlockExpression.t
      | StandardExpression of StandardExpression.t
  end

  module Function : sig
    type t = (t' * Loc.t)

    and t' = {
      id: Identifier.t option;
      params: Identifier.t list option;
      body: Expression.BlockExpression.t;
    }
  end

  module Object : sig
    type t
  end

  module VariableDeclaration : sig
    type t = (t' * Loc.t)

    and t' = {
      id: Identifier.t;
      value: t'';
      kind: var_t;
    }

    and t'' =
      | Empty
      | Identifier of Identifier.t
      | Function of Function.t
      | Expression of Expression.t
      | Literal of Literal.t
      | Object of Object.t

    and var_t =
      | Var
      | Let
      | Const
  end

  type t' = 
    | Identifier of Identifier.t
    | Expression of Expression.t
    | Object of Object.t
    | VariableDeclaration of VariableDeclaration.t
  
  type t = (t' * Loc.t)
end = Statement
*)




