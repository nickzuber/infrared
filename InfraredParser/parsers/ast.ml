module Loc = Flow_parser.Loc

module rec InfraredAst : sig
  type identifier' = string

  and unop =
    | Negate
    | Not

  and binop =
    | Plus
    | Minus
    | Multiply
    | Divide
    | Exponent
    | Modulo
    | LeftShift
    | RightShift
    | BitOr
    | BitXor
    | BitAnd
    | And
    | Or
    | In
    | InstanceOf
    | Compare of cmp

  and cmp =
    | Equal
    | NotEqual
    | GreaterThan
    | LessThan

  and property =
    | PropertyExpression of expression
    | PropertyIdentifier of identifier

  and expression' =
    | Variable of identifier
    | String of string
    | Number of int
    | Boolean of bool
    | Object of (identifier * expression) list
    | Null
    | Undefined
    | Call of expression * (expression list) (* e(e1, e2) *)
    | Access of expression * property (* e.e *)
    | Assignment of identifier * expression (* x = e *)
    | UnaryOperation of unop * expression
    | BinaryOperation of binop * expression * expression

  and statement' =
    (* Recall that VariableAssignment don't exist for our AST.
     * Any VariableAssignments will be considered as new variable
     * declarations, so we can track any type branches for free. *)
    | VariableDeclaration of identifier * expression (* var x = e *)
    | FunctionDeclaration of identifier * (identifier list) * (statement list) (* name, arguments, body *)
    | If of expression * statement * statement
    | Return of expression
    | Expression of expression
    | Block of statement list

  and expression = Loc.t * expression'
  and statement = Loc.t * statement'
  and identifier = Loc.t * identifier'
end = InfraredAst

type primative_data_type =
  | Null
  | Undefined
  | String
  | Boolean
  | Number
  | Object of (string * data_type) list (* PropertyName, Value *)
  | Array of data_type
  | Function of data_type list * data_type (* Arguments, ReturnType *)

and data_type =
  | Primative of primative_data_type
  | Generic of string (* tag *)
  | Defer of InfraredAst.identifier (* Do we need this? Might be resolved via Exec *)
  | Drill of data_type * InfraredAst.property
  | Exec of data_type * (data_type list) (* f(a, b, c) *)
  | Reduction of data_type list (* operations on expressions *)
  | Union of data_type list
  | Unknown

type environment = (string, data_type) Hashtbl.t

(* Same as InfraredAst, only this has a typed expression *)
module rec TypedInfraredAst : sig
  type typed_expression = data_type * InfraredAst.expression

  and statement' =
    (* Recall that VariableAssignment don't exist for our AST.
     * Any VariableAssignments will be considered as new variable
     * declarations, so we can track any type branches for free. *)
    | VariableDeclaration of InfraredAst.identifier * typed_expression (* var x = e *)
    | FunctionDeclaration of InfraredAst.identifier * (InfraredAst.identifier list) * (statement list) (* name, arguments, body *)
    | If of typed_expression * statement * statement
    | Return of typed_expression
    | Expression of typed_expression
    | Block of statement list

  and statement = Loc.t * statement'
end = TypedInfraredAst

type program =
  | FlowProgram of Flow_parser.Loc.t Flow_parser.Ast.program *
                   (Flow_parser.Loc.t * Flow_parser.Parser_common.Error.t) list
  | InfraredProgram of InfraredAst.statement list
  | TypedInfraredProgram of TypedInfraredAst.statement list * environment
