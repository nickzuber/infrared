module rec InfraredAst : sig
  type identifier = string

  type unop =
    | Minus
    | Not

  and binop =
    | Plus
    | And
    | Or
    | Compare of cmp

  and cmp =
    | Equal
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

type program =
  | FlowProgram of Flow_parser.Loc.t Flow_parser.Ast.program *
                   (Flow_parser.Loc.t * Flow_parser.Parser_common.Error.t) list
  | InfraredProgram of InfraredAst.statement list
