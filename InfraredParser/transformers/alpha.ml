open InfraredUtils
open Ast
module FlowAst = Flow_parser.Ast
module Loc = Flow_parser.Loc

exception Unhandled_parsing_step of string
exception Illegal_parsing_step of string
exception TransformerBinaryError
exception TransformerFunctionDeclarationError
exception TransformerVariableDeclarationError
exception TransformerExpressionError

let logger_error (message : string) (loc : Loc.t) : unit =
  Logger.error ("[InfraredParser.alpha] " ^ message) loc

let logger_error_no_loc (message : string) : unit =
  Logger.error_no_loc ("[InfraredParser.alpha] " ^ message)

module rec FunctionDeclarationTransformer : sig
  val transform : Loc.t FlowAst.Function.t -> InfraredAst.statement'
end = struct
  let rec transform (fn : Loc.t FlowAst.Function.t) : InfraredAst.statement' =
    let open FlowAst.Function in
    let name = transform_identifier_maybe fn.id in
    let params = transform_params fn.params in
    let body = transform_body fn.body in
    InfraredAst.FunctionDeclaration (name, params, body)

  and transform_body (body : Loc.t FlowAst.Function.body) : InfraredAst.statement list =
    match body with
    | BodyExpression expression ->
      let (loc, _) = expression in
      let expr = InfraredAst.Expression (ExpressionTransformer.transform_expression expression) in
      [(loc, expr)]
    | BodyBlock block -> transform_body_block block

  and transform_body_block (block : Loc.t * Loc.t FlowAst.Statement.Block.t) : InfraredAst.statement list =
    let (_, block) = block in
    List.map StatementTransformer.transform_statement block.body

  and transform_identifier_maybe (identifier_maybe : Loc.t FlowAst.Identifier.t option) : InfraredAst.identifier =
    match identifier_maybe with
    | Some id -> ExpressionTransformer.transform_identifier id
    | None -> (Loc.none, "(anonymous)")

  and transform_params (params : Loc.t FlowAst.Function.Params.t) : InfraredAst.identifier list =
    (* Ignore this location since we have the individual param locations *)
    let (_, obj) = params in
    (* @TODO skipping rest elements for now. *)
    List.map ExpressionTransformer.transform_pattern obj.params
end

and ReturnTransformer : sig
  val transform : Loc.t FlowAst.Statement.Return.t -> InfraredAst.statement'
end = struct
  let transform (obj : Loc.t FlowAst.Statement.Return.t) : InfraredAst.statement' =
    let open FlowAst.Statement.Return in
    let arg = ExpressionTransformer.transform_expression_maybe obj.argument in
    InfraredAst.Return arg
end

and IfTransformer : sig
  val transform : Loc.t FlowAst.Statement.If.t -> InfraredAst.statement'
end = struct
  let transform (obj : Loc.t FlowAst.Statement.If.t) : InfraredAst.statement' =
    let open FlowAst.Statement.If in
    let test = ExpressionTransformer.transform obj.test in
    let consequent = StatementTransformer.transform_statement obj.consequent in
    let alternate = match obj.alternate with
      | Some statement -> StatementTransformer.transform_statement statement
      | None -> (Loc.none, InfraredAst.Expression ((Loc.none, InfraredAst.Undefined)))
    in
    InfraredAst.If (test, consequent, alternate)
end

and BlockTransformer : sig
  val transform : Loc.t FlowAst.Statement.Block.t -> InfraredAst.statement'
end = struct
  let transform (block : Loc.t FlowAst.Statement.Block.t) : InfraredAst.statement' =
    let open FlowAst.Statement.Block in
    let statements' = List.map StatementTransformer.transform_statement block.body in
    InfraredAst.Block statements'
end

(* @TODO
 * We're only looking at the first declarator right now.
 * ```
 * var x = 2, y = 3
 * ```
 * We'll only process `x` *)
and VariableDeclarationTransformer : sig
  val transform : Loc.t FlowAst.Statement.VariableDeclaration.t -> InfraredAst.statement'
end = struct
  let transform (obj : Loc.t FlowAst.Statement.VariableDeclaration.t) : InfraredAst.statement' =
    let open FlowAst.Statement.VariableDeclaration in
    let declarations = obj.declarations in
    let declaration = List.hd declarations in
    let (loc, declaration') = declaration in
    if List.length declarations > 1 then
      Logger.warn "Skipping additional declarators" loc;
    let identifier = ExpressionTransformer.transform_pattern declaration'.id in
    let value = ExpressionTransformer.transform_expression_maybe declaration'.init in
    InfraredAst.VariableDeclaration (identifier, value)
end

and ExpressionTransformer : sig
  val transform : Loc.t FlowAst.Expression.t -> InfraredAst.expression
  val transform_expression : Loc.t FlowAst.Expression.t -> InfraredAst.expression
  val transform_pattern : Loc.t FlowAst.Pattern.t -> InfraredAst.identifier
  val transform_identifier : Loc.t FlowAst.Identifier.t -> InfraredAst.identifier
  val transform_expression_maybe : Loc.t FlowAst.Expression.t option -> InfraredAst.expression
  val transform_literal : FlowAst.Literal.t -> InfraredAst.expression'
end = struct
  let rec transform_pattern (pattern : Loc.t FlowAst.Pattern.t) : InfraredAst.identifier =
    let open FlowAst.Pattern in
    let (loc, pattern') = pattern in
    match pattern' with
    | Identifier id -> transform_identifier id.name
    | _ ->
      logger_error "Unhandled pattern type in function parameters" loc;
      (raise TransformerExpressionError)

  and transform_identifier (identifier : Loc.t FlowAst.Identifier.t) : InfraredAst.identifier =
    let (loc, name) = identifier in
    (loc, name)

  and transform_expression (expression : Loc.t FlowAst.Expression.t) : InfraredAst.expression =
    let open FlowAst.Expression in
    let (loc, expr) = expression in
    match expr with
    | Literal object_literal ->
      let lit = transform_literal object_literal in
      (loc, lit)
    | Identifier id ->
      let expr' = InfraredAst.Variable (transform_identifier id) in
      (loc, expr')
    | Object obj ->
      let obj' = InfraredAst.Object (transform_object_properties loc obj.properties) in
      (loc, obj')
    | Binary binary_expression ->
      let expr = transform_binary binary_expression in
      (loc, expr)
    | Assignment obj ->
      let expr = transform_assignment obj in
      (loc, expr)
    | Member obj ->
      let expr = transform_member_expression obj in
      (loc, expr)
    | Call obj ->
      let expr = transform_call_expression obj in
      (loc, expr)
    | _ ->
      logger_error "Unhandled expression type" loc;
      (raise TransformerExpressionError)

  and transform (expression : Loc.t FlowAst.Expression.t) : InfraredAst.expression =
    transform_expression expression

  and transform_call_expression (obj : Loc.t FlowAst.Expression.Call.t) : InfraredAst.expression' =
    let arguments = List.map transform_expression_or_spread obj.arguments in
    let callee = transform_expression obj.callee in
    InfraredAst.Call (callee, arguments)

  and transform_expression_or_spread (expr_or_spread : Loc.t FlowAst.Expression.expression_or_spread) : InfraredAst.expression =
    let open FlowAst.Expression in
    match expr_or_spread with
    | Expression expr -> transform_expression expr
    | Spread _ ->
      logger_error_no_loc "Unhandled SpreadExpression";
      (raise TransformerExpressionError)

  and transform_member_expression (obj : Loc.t FlowAst.Expression.Member.t) : InfraredAst.expression' =
    let object_expr = transform_expression obj._object in
    let property_expr = transform_member_property obj.property in
    InfraredAst.Access (object_expr, property_expr)

  and transform_member_property (prop : Loc.t FlowAst.Expression.Member.property) : InfraredAst.property =
    let open FlowAst.Expression.Member in
    match prop with
    | PropertyIdentifier id -> PropertyIdentifier (transform_identifier id)
    | PropertyExpression expr -> PropertyExpression (transform_expression expr)
    | PropertyPrivateName _name ->
      logger_error_no_loc "Unhandled SpreadProperty in object";
      (raise TransformerExpressionError)

  and transform_object_properties (loc : Loc.t) (properties : Loc.t FlowAst.Expression.Object.property list)
    : (InfraredAst.identifier * InfraredAst.expression) list =
    List.map (fun property ->
        let open FlowAst.Expression.Object in
        match property with
        | Property prop -> transform_object_property prop
        | SpreadProperty _ ->
          logger_error "Unhandled SpreadProperty in object" loc;
          (raise TransformerExpressionError)
      ) properties

  and transform_object_property (property : Loc.t FlowAst.Expression.Object.Property.t) : (InfraredAst.identifier * InfraredAst.expression) =
    let open FlowAst.Expression.Object.Property in
    let (loc, prop) = property in
    let transform_key key : InfraredAst.identifier =
      match key with
      | Literal lit ->
        let (loc, lit) = lit in
        let lit_expr = transform_literal lit in
        let str = Printf.sprintf "%s"
            (string_of_infrared_literal lit_expr)
        in
        (loc, str)
      | Identifier id -> transform_identifier id
      | Computed (loc, _) -> (loc, "<#computed_value>")
      | PrivateName _ ->
        logger_error "Unhandled PrivateName in object property" loc;
        (raise TransformerExpressionError)
    in
    match prop with
    | Init obj ->
      let key = transform_key obj.key in
      let value = transform_expression obj.value in
      (key, value)
    | _ ->
      logger_error "Unhandled Property type in object property" loc;
      (raise TransformerExpressionError)

  and transform_expression_maybe (expression_maybe : Loc.t FlowAst.Expression.t option) : InfraredAst.expression =
    match expression_maybe with
    | Some expr -> transform_expression expr
    | None -> (Loc.none, Undefined)

  and transform_literal (object_literal : FlowAst.Literal.t) : InfraredAst.expression' =
    match object_literal.value with
    | Boolean true -> InfraredAst.Boolean true
    | Boolean false -> InfraredAst.Boolean false
    | Null -> InfraredAst.Null
    | Number n -> InfraredAst.Number (int_of_float n)
    | String s -> InfraredAst.String s
    | RegExp regex_object -> InfraredAst.String ("/" ^ regex_object.pattern ^ "/")

  and string_of_infrared_literal literal : string =
    let open InfraredAst in
    match literal with
    | Boolean true -> "true"
    | Boolean false -> "false"
    | Null -> "Null"
    | Undefined -> "Undefined"
    | Number n -> string_of_int n
    | String s -> s
    | _ -> "<#unknown>"

  and transform_assignment (obj : Loc.t FlowAst.Expression.Assignment.t) =
    let open FlowAst.Expression.Assignment in
    let left = ExpressionTransformer.transform_pattern obj.left in
    let right = ExpressionTransformer.transform_expression obj.right in
    InfraredAst.Assignment (left, right)

  and transform_binary (obj : Loc.t FlowAst.Expression.Binary.t) =
    let open FlowAst.Expression.Binary in
    let left = transform_expression obj.left in
    let right = transform_expression obj.right in
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

and StatementTransformer : sig
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
    let (loc, statement) = flow_statement in
    match statement with
    | VariableDeclaration obj ->
      let statement = VariableDeclarationTransformer.transform obj in
      (loc, statement)
    | FunctionDeclaration fn ->
      let statement = FunctionDeclarationTransformer.transform fn in
      (loc, statement)
    | Return obj ->
      let statement = ReturnTransformer.transform obj in
      (loc, statement)
    | If obj ->
      let statement = IfTransformer.transform obj in
      (loc, statement)
    | Block obj ->
      let statement = BlockTransformer.transform obj in
      (loc, statement)
    | Expression expression_object ->
      let expression = ExpressionTransformer.transform_expression expression_object.expression in
      (loc, InfraredAst.Expression expression)
    | _ -> raise (Unhandled_parsing_step "#<unhandled_FlowStatement_to_InfraredStatement_conversion>")
end
