open Core.Std
open Yojson

(* We don't have exhaustiveness in the NativeEncoder -> InfraredAst conversion. *)
exception Malformed_json_ast of string

(* Exhaustiveness with defer *)
exception Unimplemented of string

exception Illegal_argument_type_error of string

module rec NativeEncoder : sig
  include module type of Yojson.Basic
  val parse : string -> Yojson.Basic.json
  val to_string_exn : string -> Yojson.Basic.json -> string
end = struct
  include Yojson.Basic
  let parse file = Basic.from_file file

  let to_string_exn key node =
    try
      node |> Basic.Util.member key |> Basic.Util.to_string
    with
    | _ -> raise
             (Illegal_argument_type_error
                "`to_string_exn` expects the json node to be a string.")
end

and InfraredEncoder : sig
  val parse_statement : NativeEncoder.json -> InfraredAst.statement list
  val parse_items : NativeEncoder.json -> InfraredAst.statement list
end = struct
  let parse_statement (node : NativeEncoder.json) : InfraredAst.statement list =
    let module SP = StatementParser in
    let module I = InfraredAst in
    let module N = NativeEncoder in
    let t = N.to_string_exn "type" node in
    match t with
    | "EmptyStatement" -> [I.Skip]
    | "VariableDeclarationStatement" ->
      let variable_declarations = SP.parse_declaration node in
      variable_declarations
    | "FunctionDeclaration" ->
      let function_expression = SP.parse_function node in
      [function_expression]
    | _ as unhandled_type -> raise (Unimplemented unhandled_type)

  (* Derive an InfraredAst from a Yojson encoded Shift AST. *)
  let rec parse_items (node : NativeEncoder.json) : InfraredAst.statement list =
    match node with
    | `List nodes ->
      let _ = Printf.printf "list %d\n" (List.length nodes) in
      List.fold_left nodes ~init:[] ~f:(fun acc node ->
          let nodes = parse_statement node in
          acc @ nodes)
    | `Assoc _ | `Bool _  | `String _  | `Float _  | `Int _  | `Null
      -> raise (Malformed_json_ast "Should have been a list")
end

and StatementParser : sig
  val parse_function : NativeEncoder.json -> InfraredAst.statement
  val parse_declaration : NativeEncoder.json -> InfraredAst.statement list
end = struct
  let parse_function (node : NativeEncoder.json) : InfraredAst.statement =
    let module I = InfraredAst in
    I.Skip

  let parse_declaration (node : NativeEncoder.json) : InfraredAst.statement list =
    let module U = NativeEncoder.Util in
    let module EP = ExpressionParser in
    let module IP = IdentifierParser in
    let module I = InfraredAst in
    let declarators = node
                      |> U.member "declaration"
                      |> U.member "declarators"
                      |> U.to_list
    in
    (* For each delcarator, create a variable declaration statement. *)
    let declarations = List.fold_left
        declarators
        ~init:[]
        ~f:(fun acc declarator ->
            let binding : InfraredAst.identifier = declarator |> U.member "binding" |> IP.parse_binding in
            let init : InfraredAst.expression = declarator |> U.member "init" |> EP.parse_expression in
            let declaration = I.Declaration (binding, init) in
            declaration :: acc)
    in
    (* This should be reversed here to counter the backwards appending to the
       list. This probably doesn't matter, but we can remove later. *)
    List.rev declarations
end

and ExpressionParser : sig
  val parse_expression : NativeEncoder.json -> InfraredAst.expression
end = struct
  let parse_expression (node : NativeEncoder.json) : InfraredAst.expression =
    let module U = NativeEncoder.Util in
    let module N = NativeEncoder in
    let module I = InfraredAst in
    let t = N.to_string_exn "type" node in
    match t with
    | "LiteralNumericExpression" ->
      let _value = node |> U.member "value" in
      I.Primitive I.P_number
    | _ -> raise (Unimplemented "Expression")
end

and IdentifierParser : sig
  val parse_binding : NativeEncoder.json -> InfraredAst.identifier
end = struct
  (* We always want to spit out a string identifier from this routine. Even in
     the more complex types of assignments (think nested destructuring), at the
     end of the day we just want a single identifer. The init will always
     drive the type. *)
  let parse_binding (node : NativeEncoder.json) : InfraredAst.identifier =
    let module U = NativeEncoder.Util in
    let module N = NativeEncoder in
    let module I = InfraredAst in
    let t = N.to_string_exn "type" node in
    match t with
    | "BindingIdentifier" ->
      let name = node |> U.member "name" |> U.to_string in
      I.Identifer name
    | "ObjectBinding" ->
      begin
        let properties = node |> U.member "properties" in
        let properties_t = properties |> N.to_string_exn "type" in
        match properties_t with
        | "BindingPropertyIdentifier" -> raise (Unimplemented "BindingPropertyIdentifier")
        | "BindingPropertyProperty" -> raise (Unimplemented "BindingPropertyProperty")
        | _ -> raise (Unimplemented "Expression")
      end
    | "ArrayBinding" -> raise (Unimplemented "ArrayBinding")
    | _ -> raise (Unimplemented "Expression")
end

(*

BindingIdentifier
  name: string

ObjectBinding
  properties: BindingProperty
    BindingPropertyIdentifier
      binding: BindingIdentifier
      init: Expression
    BindingPropertyProperty
      name: PropertyName
        ComputedPropertyName
          expression: Expression
        StaticPropertyName
          value: string

ArrayBinding
  ...

*)
