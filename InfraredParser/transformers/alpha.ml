open InfraredUtils
open Ast
module FlowAst = Flow_parser.Ast
module Loc = Flow_parser.Loc

exception Unhandled_parsing_step of string
exception Illegal_parsing_step of string
exception TransformerBinaryError
exception TransformerFunctionDeclarationError
exception TransformerVariableDeclarationError


module rec BinaryTransformer : sig
  val transform : Loc.t FlowAst.Expression.Binary.t -> InfraredAst.expression
end = struct
  let rec transform (obj : Loc.t FlowAst.Expression.Binary.t) =
    let open FlowAst.Expression.Binary in
    let left = TransformerHelper.transform_expression obj.left in
    let right = TransformerHelper.transform_expression obj.right in
    let operator = transform_assignment_op obj.operator in
    InfraredAst.BinaryOperation
      (operator, left, right)

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

end

and FunctionDeclarationTransformer : sig
  val transform : Loc.t FlowAst.Function.t -> InfraredAst.statement
end = struct
  let rec transform (fn : Loc.t FlowAst.Function.t) : InfraredAst.statement =
    let open FlowAst.Function in
    let name = transform_identifier_maybe fn.id in
    let params = transform_params fn.params in
    let body = transform_body fn.body in
    InfraredAst.FunctionDeclaration (name, params, body)

  and transform_body (body : Loc.t FlowAst.Function.body) : InfraredAst.statement list =
    match body with
    | BodyExpression expression -> [InfraredAst.Expression (TransformerHelper.transform_expression expression)]
    | BodyBlock block -> transform_body_block block

  and transform_body_block (block : Loc.t * Loc.t FlowAst.Statement.Block.t) : InfraredAst.statement list =
    let (_, block) = block in
    List.map Transformer.transform_statement block.body

  and transform_identifier_maybe (identifier_maybe : Loc.t FlowAst.Identifier.t option) : string =
    match identifier_maybe with
    | Some id -> TransformerHelper.transform_identifier id
    | None -> "(anonymous)"

  and transform_params (params : Loc.t FlowAst.Function.Params.t) : InfraredAst.identifier list =
    let (_, obj) = params in
    (* @TODO skipping rest elements for now. *)
    List.map TransformerHelper.transform_pattern obj.params
end

and ReturnTransformer : sig
  val transform : Loc.t FlowAst.Statement.Return.t -> InfraredAst.statement
end = struct
  let transform (obj : Loc.t FlowAst.Statement.Return.t) : InfraredAst.statement =
    let open FlowAst.Statement.Return in
    let arg = TransformerHelper.transform_expression_maybe obj.argument in
    InfraredAst.Return arg
end

(* @TODO
 * We're only looking at the first declarator right now.
 * ```
 * var x = 2, y = 3
 * ```
 * We'll only process `x` *)
and VariableDeclarationTransformer : sig
  val transform : Loc.t FlowAst.Statement.VariableDeclaration.t -> InfraredAst.statement
end = struct
  let rec transform (obj : Loc.t FlowAst.Statement.VariableDeclaration.t) =
    let open FlowAst.Statement.VariableDeclaration in
    let declarations = obj.declarations in
    let declaration = List.hd declarations in
    let (loc, declaration') = declaration in
    if List.length declarations > 1 then
      Logger.warn "Skipping additional declarators" loc;
    let identifier = string_of_pattern declaration'.id in
    let value = TransformerHelper.transform_expression_maybe declaration'.init in
    InfraredAst.VariableDeclaration (identifier, value)

  and string_of_pattern (id : Loc.t FlowAst.Pattern.t) : string =
    let open FlowAst.Pattern in
    let (loc, pattern) = id in
    match pattern with
    | Identifier id -> TransformerHelper.transform_identifier id.name
    | _ ->
      Logger.warn "Unhandled variable declaration identifier type" loc;
      "#<unhandled_identifer_type>"
end

and AssignmentTransformer : sig
  val transform : Loc.t FlowAst.Expression.Assignment.t -> InfraredAst.expression
end = struct
  let transform (obj : Loc.t FlowAst.Expression.Assignment.t) =
    let open FlowAst.Expression.Assignment in
    let left = TransformerHelper.transform_pattern obj.left in
    let right = TransformerHelper.transform_expression obj.right in
    InfraredAst.Assignment (left, right)
end

and TransformerHelper : sig
  val transform_pattern : Loc.t FlowAst.Pattern.t -> string
  val transform_identifier : Loc.t FlowAst.Identifier.t -> string
  val transform_expression : Loc.t FlowAst.Expression.t -> InfraredAst.expression
  val transform_expression_maybe : Loc.t FlowAst.Expression.t option -> InfraredAst.expression
  val transform_literal : FlowAst.Literal.t -> InfraredAst.expression
end = struct
  let rec transform_pattern (pattern : Loc.t FlowAst.Pattern.t) : string =
    let open FlowAst.Pattern in
    let (loc, pattern') = pattern in
    match pattern' with
    | Identifier id -> transform_identifier id.name
    | _ ->
      Logger.error "Unhandled pattern type in function parameters" loc;
      (raise TransformerFunctionDeclarationError)

  and transform_identifier (identifier : Loc.t FlowAst.Identifier.t) : string =
    let (_, name) = identifier in
    name

  and transform_expression (expression : Loc.t FlowAst.Expression.t) : InfraredAst.expression =
    let open FlowAst.Expression in
    let (loc, expr) = expression in
    match expr with
    | Literal object_literal -> transform_literal object_literal
    | Identifier id -> InfraredAst.Variable (transform_identifier id)
    | _ ->
      Logger.error "Unhandled expression type" loc;
      (raise TransformerFunctionDeclarationError)

  and transform_expression_maybe (expression_maybe : Loc.t FlowAst.Expression.t option) : InfraredAst.expression =
    match expression_maybe with
    | Some expr -> transform_expression expr
    | None -> Undefined

  and transform_literal (object_literal : FlowAst.Literal.t) : InfraredAst.expression =
    match object_literal.value with
    | Boolean true -> InfraredAst.Boolean true
    | Boolean false -> InfraredAst.Boolean false
    | Null -> InfraredAst.Null
    | Number n -> InfraredAst.Number (int_of_float n)
    | String s -> InfraredAst.String s
    | RegExp regex_object -> InfraredAst.String ("/" ^ regex_object.pattern ^ "/")
end

and Transformer : sig
  val transform : program -> program
  val transform_statement : Loc.t FlowAst.Statement.t -> InfraredAst.statement
end = struct
  let rec transform (prog : program) : program =
    let flow_program = match prog with
      | FlowProgram ((_loc, flow_statements, _comments), _errs) ->
        let statements = infrared_statements_of_flow_statements flow_statements in
        InfraredProgram (statements)
      | _ -> raise
               (Illegal_parsing_step
                  "Expected FlowProgram at VariableDeclarationTransformer#transforming")
    in
    flow_program

  and transform_statement (flow_statement : Loc.t FlowAst.Statement.t) : InfraredAst.statement =
    infrared_statement_of_flow_statement flow_statement

  and infrared_statements_of_flow_statements (flow_statements : Loc.t FlowAst.Statement.t list)
    : InfraredAst.statement list =
    let infrared_statements = List.map
        (fun statement -> infrared_statement_of_flow_statement statement)
        flow_statements
    in
    infrared_statements

  and infrared_statement_of_flow_statement (flow_statement : Loc.t FlowAst.Statement.t)
    : InfraredAst.statement =
    let open FlowAst.Statement in
    (* let open InfraredAst in *)
    let (_loc, statement) = flow_statement in
    match statement with
    | VariableDeclaration obj -> VariableDeclarationTransformer.transform obj
    | FunctionDeclaration fn -> FunctionDeclarationTransformer.transform fn
    | Return obj -> ReturnTransformer.transform obj
    | Expression expression_object ->
      let open FlowAst.Expression in
      let (_loc, expr) = expression_object.expression in
      let expression = (
        match expr with
        | Binary binary_expression -> BinaryTransformer.transform binary_expression
        | Assignment obj -> AssignmentTransformer.transform obj
        | _ -> raise (Unhandled_parsing_step "#<Unhandled FlowExpression>")
      )
      in
      InfraredAst.Expression expression
    | _ -> raise (Unhandled_parsing_step "#<Unhandled FlowStatement>")
end
