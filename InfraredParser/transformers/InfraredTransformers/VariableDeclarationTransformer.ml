open InfraredUtils
open Ast
module FlowAst = Flow_parser.Ast
module Loc = Flow_parser.Loc


(* @TODO
 * We're only looking at the first declarator right now.
 * ```
 * var x = 2, y = 3
 * ```
 * We'll only process `x` *)
module VariableDeclarationTransformer = struct
  let rec transform (obj : Loc.t FlowAst.Statement.VariableDeclaration.t) =
    let open FlowAst.Statement.VariableDeclaration in
    let open InfraredAst in
    let declarations = obj.declarations in
    let declaration = List.hd declarations in
    let (loc, declaration') = declaration in
    if List.length declarations > 1 then
      Logger.warn "Skipping additional declarators" loc;
    let identifier = string_of_pattern declaration'.id in
    InfraredAst.VariableDeclaration (identifier, Null)

  and string_of_pattern (id : Loc.t FlowAst.Pattern.t) : string =
    let open FlowAst.Pattern in
    let (loc, pattern) = id in
    match pattern with
    | Identifier id -> string_of_identifier id.name
    | _ ->
      Logger.warn "Unhandled variable declaration identifier type" loc;
      "#<unhandled_identifer_type>"

  and string_of_identifier (identifier : Loc.t FlowAst.Identifier.t) : string =
    let (_, name) = identifier in
    name

end
