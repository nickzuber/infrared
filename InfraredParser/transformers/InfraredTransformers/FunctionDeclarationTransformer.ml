(* open InfraredUtils
   open Ast
   module FlowAst = Flow_parser.Ast
   module Loc = Flow_parser.Loc


   exception TransformerFunctionDeclarationError

   module FunctionDeclarationTransformer = struct
   let rec transform (fn : Loc.t FlowAst.Function.t) : InfraredAst.statement =
    let open FlowAst.Function in
    let name = transform_identifier_maybe fn.id in
    let params = transform_params fn.params in
    let body = transform_body fn.body in
    InfraredAst.FunctionDeclaration (name, params, body)

   and transform_body (body : Loc.t FlowAst.Function.body) : InfraredAst.statement list =
    match body with
    | BodyExpression expression -> [InfraredAst.Expression (transform_expression expression)]
    | BodyBlock block -> transform_body_block block

   and transform_body_block (block : Loc.t * Loc.t FlowAst.Statement.Block.t) : InfraredAst.statement list =
    let (_, block) = block in
    List.map transform_statement block.body

   and transform_identifier_maybe (identifier_maybe : Loc.t FlowAst.Identifier.t option) : string =
    match identifier_maybe with
    | Some id -> transform_identifier id
    | None -> "(anonymous)"

   and transform_identifier (identifier : Loc.t FlowAst.Identifier.t) : string =
    let (_, name) = identifier in
    name

   and transform_params (params : Loc.t FlowAst.Function.Params.t) : InfraredAst.identifier list =
    let (_, obj) = params in
    (* @TODO skipping rest elements for now. *)
    List.map transform_patterns obj.params

   and transform_patterns (pattern : Loc.t FlowAst.Pattern.t) : string =
    let open FlowAst.Pattern in
    let (loc, pattern') = pattern in
    match pattern' with
    | Identifier id -> transform_identifier id.name
    | _ ->
      Logger.error "Unhandled pattern type in function parameters" loc;
      (raise TransformerFunctionDeclarationError)

   and transform_expression (expression : Loc.t FlowAst.Expression.t) : InfraredAst.expression =
    let open FlowAst.Expression in
    let (loc, expr) = expression in
    match expr with
    | Literal object_literal -> transform_literal object_literal
    | _ ->
      Logger.error "Unhandled expression type" loc;
      (raise TransformerFunctionDeclarationError)

   and transform_literal (object_literal : FlowAst.Literal.t) : InfraredAst.expression =
    match object_literal.value with
    | Boolean true -> InfraredAst.Boolean true
    | Boolean false -> InfraredAst.Boolean false
    | Null -> InfraredAst.Null
    | Number n -> InfraredAst.Number (int_of_float n)
    | String s -> InfraredAst.String s
    | RegExp regex_object -> InfraredAst.String ("/" ^ regex_object.pattern ^ "/")

   end *)
