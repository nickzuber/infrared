(* open InfraredUtils
   open Ast
   module FlowAst = Flow_parser.Ast
   module Loc = Flow_parser.Loc


   exception TransformerVariableDeclarationError


   (* @TODO
 * We're only looking at the first declarator right now.
 * ```
 * var x = 2, y = 3
 * ```
 * We'll only process `x` *)
   module VariableDeclarationTransformer = struct
   let rec transform (obj : Loc.t FlowAst.Statement.VariableDeclaration.t) =
    let open FlowAst.Statement.VariableDeclaration in
    let declarations = obj.declarations in
    let declaration = List.hd declarations in
    let (loc, declaration') = declaration in
    if List.length declarations > 1 then
      Logger.warn "Skipping additional declarators" loc;
    let identifier = string_of_pattern declaration'.id in
    let value = transform_expression_maybe declaration'.init in
    InfraredAst.VariableDeclaration (identifier, value)

   and string_of_pattern (id : Loc.t FlowAst.Pattern.t) : string =
    let open FlowAst.Pattern in
    let (loc, pattern) = id in
    match pattern with
    | Identifier id -> transform_identifier id.name
    | _ ->
      Logger.warn "Unhandled variable declaration identifier type" loc;
      "#<unhandled_identifer_type>"

   and transform_identifier (identifier : Loc.t FlowAst.Identifier.t) : string =
    let (_, name) = identifier in
    name

   and transform_expression_maybe (expression_maybe : Loc.t FlowAst.Expression.t option)
    : InfraredAst.expression =
    match expression_maybe with
    | Some expression -> transform_expression expression
    | None -> Undefined

   and transform_expression (expression : Loc.t FlowAst.Expression.t) : InfraredAst.expression =
    let open FlowAst.Expression in
    let (loc, expr) = expression in
    match expr with
    | Literal object_literal -> transform_literal object_literal
    | _ ->
      Logger.error "Unhandled expression type" loc;
      (raise TransformerVariableDeclarationError)

   and transform_literal (object_literal : FlowAst.Literal.t) : InfraredAst.expression =
    match object_literal.value with
    | Boolean true -> InfraredAst.Boolean true
    | Boolean false -> InfraredAst.Boolean false
    | Null -> InfraredAst.Null
    | Number n -> InfraredAst.Number (int_of_float n)
    | String s -> InfraredAst.String s
    | RegExp regex_object -> InfraredAst.String ("/" ^ regex_object.pattern ^ "/")

   end *)
