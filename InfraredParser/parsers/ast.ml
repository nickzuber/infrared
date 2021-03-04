module rec InfraredAst : sig
  type identifier = string

  type unop =
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

  and expression =
    | Variable of identifier
    | String of string
    | Number of int
    | Boolean of bool
    | Null
    | Undefined
    | Access of expression * expression (* e.e *)
    | Assignment of identifier * expression (* x = e *)
    | UnaryOperation of unop * expression
    | BinaryOperation of binop * expression * expression

  and statement =
    | VariableDeclaration of identifier * expression (* var x = e *)
    | FunctionDelcaration of identifier * (identifier list) * (statement list) (* name, arguments, body *)
    | If of expression * expression * expression
    | Expression of expression
end = InfraredAst

type data_type =
  | Null
  | Undefined
  | String
  | Boolean
  | Number
  | Object of (string * data_type) list (* PropertyName, Value *)
  | Array of data_type
  | Function of data_type list * data_type (* Arguments, ReturnType *)

type environment = (string, data_type) Hashtbl.t

type program =
  | FlowProgram of Flow_parser.Loc.t Flow_parser.Ast.program *
                   (Flow_parser.Loc.t * Flow_parser.Parser_common.Error.t) list
  | InfraredProgram of InfraredAst.statement list * environment
