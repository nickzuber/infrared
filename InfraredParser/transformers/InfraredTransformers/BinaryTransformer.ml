(* open InfraredUtils
   open Ast
   module FlowAst = Flow_parser.Ast
   module Loc = Flow_parser.Loc


   exception TransformerBinaryError

   module BinaryTransformer = struct
   let rec transform (obj : Loc.t FlowAst.Expression.Binary.t) =
    let open FlowAst.Expression.Binary in
    let left = transform_expression obj.left in
    let right = transform_expression obj.right in
    let operator = transform_assignment_op obj.operator in
    InfraredAst.Expression
      (InfraredAst.BinaryOperation
         (operator, left, right))

   and transform_expression (expression : Loc.t FlowAst.Expression.t) : InfraredAst.expression =
    let open FlowAst.Expression in
    let (loc, expr) = expression in
    match expr with
    | Literal object_literal -> transform_literal object_literal
    | Identifier id -> transform_identifier id
    | _ ->
      Logger.error "Unhandled expression type" loc;
      (raise TransformerBinaryError)

   and transform_identifier (identifier : Loc.t FlowAst.Identifier.t) : InfraredAst.expression =
    let (_, name) = identifier in
    InfraredAst.Variable name

   and transform_assignment_op op =
    let open FlowAst.Expression.Binary in
    match op with
    | Equal -> InfraredAst.Compare Equal
    | NotEqual -> InfraredAst.Compare NotEqual
    | StrictEqual -> InfraredAst.Compare Equal
    | StrictNotEqual -> InfraredAst.Compare NotEqual
    | LessThan -> InfraredAst.Compare LessThan
    | LessThanEqual -> InfraredAst.Compare LessThan
    | GreaterThan -> InfraredAst.Compare GreaterThan
    | GreaterThanEqual -> InfraredAst.Compare GreaterThan
    | LShift -> InfraredAst.LeftShift
    | RShift -> InfraredAst.RightShift
    | RShift3 -> InfraredAst.RightShift
    | Plus -> InfraredAst.Plus
    | Minus -> InfraredAst.Minus
    | Mult -> InfraredAst.Multiply
    | Exp -> InfraredAst.Exponent
    | Div -> InfraredAst.Divide
    | Mod -> InfraredAst.Modulo
    | BitOr -> InfraredAst.BitOr
    | Xor -> InfraredAst.BitXor
    | BitAnd -> InfraredAst.BitAnd
    | In -> InfraredAst.In
    | Instanceof -> InfraredAst.InstanceOf

   and transform_literal (object_literal : FlowAst.Literal.t) : InfraredAst.expression =
    match object_literal.value with
    | Boolean true -> InfraredAst.Boolean true
    | Boolean false -> InfraredAst.Boolean false
    | Null -> InfraredAst.Null
    | Number n -> InfraredAst.Number (int_of_float n)
    | String s -> InfraredAst.String s
    | RegExp regex_object -> InfraredAst.String ("/" ^ regex_object.pattern ^ "/")

   end *)
