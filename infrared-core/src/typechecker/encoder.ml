open Core.Std
open Yojson

(* We don't have exhaustiveness in the NativeEncoder -> InfraredAst conversion. *)
exception Unhandled_statement_type of string
exception Malformed_json_ast of string

module rec NativeEncoder : sig
  include module type of Yojson.Basic
  val parse : string -> Yojson.Basic.json
  val type_of_json : string -> Yojson.Basic.json -> string
end = struct
  include Yojson.Basic
  let parse file = Basic.from_file file

  let type_of_json key node =
    let t_json = Basic.Util.member "type" node in
    match t_json with
    | `String t_string -> t_string
    | `Assoc _ | `Bool _  | `List _  | `Float _  | `Int _  | `Null
      -> raise (Malformed_json_ast "Should have been a string")
end

and InfraredEncoder : sig
  val parse_statement : NativeEncoder.json -> InfraredAst.statement
end = struct
  let parse_statement (node : NativeEncoder.json) : InfraredAst.statement =
    let module S = StatementParser in
    let module I = InfraredAst in
    let module N = NativeEncoder in
    let t = N.type_of_json "type" node in
    match t with
    | "EmptyStatement" -> I.Skip
    | "VariableDeclarationStatement" ->
      let declaration_expression = S.declaration node in
      I.Declaration declaration_expression
    | "FunctionDeclaration" ->
      let function_expression = S.function node in
      I.Expression function_expression
    | _ as unhandled_type -> raise (Unhandled_statement_type unhandled_type)

  (* Derive an InfraredAst from a Shift Ast encoded in Yojson. *)
  let rec parse_items (node : NativeEncoder.json) : InfraredAst.statement list =
    match node with
    | `List nodes ->
      let _ = Printf.printf "list %d\n" (List.length nodes) in
      List.map nodes ~f:(fun node -> parse_statement node)
    | `Assoc _ | `Bool _  | `String _  | `Float _  | `Int _  | `Null
      -> raise (Malformed_json_ast "Should have been a list")
end

and StatementParser : sig
  val : function -> NativeEncoder.json -> InfraredAst.expression
  val : declaration -> NativeEncoder.json -> InfraredAst.expression
end = struct
  let function (node : NativeEncoder.json) : InfraredAst.expression =
    InfraredAst.Primitive InfraredAst.P_string

  let declaration (node : NativeEncoder.json) : InfraredAst.expression =
    InfraredAst.Primitive InfraredAst.P_string
end
