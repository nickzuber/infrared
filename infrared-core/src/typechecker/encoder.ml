open Core.Std
open Yojson
module StandardInfraredAst = InfraredAst.StandardInfraredAst

(* We don't have exhaustiveness in the NativeEncoder -> StandardInfraredAst conversion. *)
exception Malformed_json_ast of string

(* Exhaustiveness with deferring *)
exception Unimplemented of string

exception Illegal_argument_type_error of string

let working_file = ref "undefined"

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
  val parse_items : fileName:string -> NativeEncoder.json -> StandardInfraredAst.statement list
  val parse_statement : NativeEncoder.json -> StandardInfraredAst.statement list
end = struct
  let parse_statement (node : NativeEncoder.json) : StandardInfraredAst.statement list =
    let module SP = StatementParser in
    let module I = StandardInfraredAst in
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
    | "Import" -> [I.Skip]
    | "ClassDeclaration" -> [I.Skip]
    | "Export" -> [I.Skip]
    | "ExportDefault" -> [I.Skip]
    | "ExportLocals" -> [I.Skip]
    | "ExportFrom" -> [I.Skip]
    | _ as unhandled_type ->
      let reason = Printf.sprintf "%s" unhandled_type in
      let (line, column, length) = Utils.destructure node in
      let err = Error_handler.exposed_error
          ~source:(!working_file)
          ~loc_line:line
          ~loc_column:column
          ~loc_length:length
          ~msg:reason
          ~reason:reason in
      raise (Unimplemented err)

  (* Derive an StandardInfraredAst from a Yojson encoded Shift AST. *)
  let rec parse_items ~(fileName : string) (node : NativeEncoder.json) : StandardInfraredAst.statement list =
    working_file := fileName;
    match node with
    | `List nodes ->
      List.fold_left nodes ~init:[] ~f:(fun acc node ->
          let nodes = parse_statement node in
          acc @ nodes)
    | `Assoc _ | `Bool _  | `String _  | `Float _  | `Int _  | `Null
      -> raise (Malformed_json_ast "Should have been a List")
end

and StatementParser : sig
  val parse_function : NativeEncoder.json -> StandardInfraredAst.statement
  val parse_declaration : NativeEncoder.json -> StandardInfraredAst.statement list
end = struct
  let parse_function (node : NativeEncoder.json) : StandardInfraredAst.statement =
    let module I = StandardInfraredAst in
    I.Skip  (* @TODO *)

  let parse_declaration (node : NativeEncoder.json) : StandardInfraredAst.statement list =
    let module U = NativeEncoder.Util in
    let module EP = ExpressionParser in
    let module IP = IdentifierParser in
    let module I = StandardInfraredAst in
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
            let binding : StandardInfraredAst.identifier = declarator |> U.member "binding" |> IP.parse_binding in
            let init : StandardInfraredAst.expression = declarator |> U.member "init" |> EP.parse_expression in
            let declaration = I.Declaration (binding, init) in
            declaration :: acc)
    in
    (* This should be reversed here to counter the backwards appending to the
       list. This probably doesn't matter, but we can remove later. *)
    List.rev declarations
end

and ExpressionParser : sig
  val parse_data_properties : NativeEncoder.json list -> (string * StandardInfraredAst.expression) list
  val parse_property_name : NativeEncoder.json -> string
  val parse_expression : NativeEncoder.json -> StandardInfraredAst.expression
end = struct
  let rec parse_data_properties (nodes : NativeEncoder.json list) : (string * StandardInfraredAst.expression) list =
    let module U = NativeEncoder.Util in
    let module N = NativeEncoder in
    let module I = StandardInfraredAst in
    List.fold_left nodes ~init:[] ~f:(fun acc node ->
        let name = node |> U.member "name" in
        let expression = node |> U.member "expression" in
        let property_name = parse_property_name name in
        let property_expression = parse_expression expression in
        let member = (property_name, property_expression) in
        member :: acc)

  and parse_property_name (node : NativeEncoder.json) : string =
    let module U = NativeEncoder.Util in
    let module N = NativeEncoder in
    let module I = StandardInfraredAst in
    let t = N.to_string_exn "type" node in
    match t with
    | "StaticPropertyName" ->
      let value = node |> U.member "value" |> U.to_string in
      value
    | "ComputedPropertyName" ->
      let expr_node = node |> U.member "expression" in
      let expr = parse_expression expr_node in
      "ComputedPropertyName, PARSED EXPR (need a way to get this into a string)"
    | _ ->
      let reason = Printf.sprintf "ExpressionParser.parse_property_name: %s" t in
      let (line, column, length) = Utils.destructure node in
      let err = Error_handler.exposed_error
          ~source:(!working_file)
          ~loc_line:line
          ~loc_column:column
          ~loc_length:length
          ~msg:reason
          ~reason:reason in
      raise (Unimplemented err)

  and parse_expression (node : NativeEncoder.json) : StandardInfraredAst.expression =
    let module U = NativeEncoder.Util in
    let module N = NativeEncoder in
    let module I = StandardInfraredAst in
    let t = N.to_string_exn "type" node in
    match t with
    | "LiteralNumericExpression" ->
      let _value = node |> U.member "value" in
      I.Primitive I.P_number
    | "LiteralStringExpression" ->
      let _value = node |> U.member "value" in
      I.Primitive I.P_string
    | "LiteralNullExpression" ->
      I.Primitive I.P_null
    | "ObjectExpression" ->
      let properties = node |> U.member "properties" |> U.to_list in
      let data_properties = parse_data_properties properties in
      I.Primitive (I.P_object data_properties)
    | _ ->
      let reason = Printf.sprintf "ExpressionParser.parse_expression: %s" t in
      let (line, column, length) = Utils.destructure node in
      let err = Error_handler.exposed_error
          ~source:(!working_file)
          ~loc_line:line
          ~loc_column:column
          ~loc_length:length
          ~msg:reason
          ~reason:reason in
      raise (Unimplemented err)
end

and IdentifierParser : sig
  val parse_binding : NativeEncoder.json -> StandardInfraredAst.identifier
end = struct
  (* We always want to spit out a string identifier from this routine. Even in
     the more complex types of assignments (think nested destructuring), at the
     end of the day we just want a single identifer. The init will always
     drive the type. *)
  let parse_binding (node : NativeEncoder.json) : StandardInfraredAst.identifier =
    let module U = NativeEncoder.Util in
    let module N = NativeEncoder in
    let module I = StandardInfraredAst in
    let t = N.to_string_exn "type" node in
    match t with
    | "BindingIdentifier" ->
      let name = node |> U.member "name" |> U.to_string in
      let identifier = InfraredAst.StandardIdentifier.Identifier name in
      I.(identifier)
    | "ObjectBinding" ->
      begin
        let properties = node |> U.member "properties" in
        let properties_t = properties |> N.to_string_exn "type" in
        match properties_t with
        | "BindingPropertyIdentifier" -> raise (Unimplemented "BindingPropertyIdentifier")
        | "BindingPropertyProperty" -> raise (Unimplemented "BindingPropertyProperty")
        | _ -> raise (Unimplemented "Properties")
      end
    | "ArrayBinding" -> raise (Unimplemented "ArrayBinding")
    | _ -> raise (Unimplemented "Expression")
end

and Utils : sig
  val destructure : NativeEncoder.json -> int * int * int
end = struct
  let destructure (node : NativeEncoder.json) =
    let module U = NativeEncoder.Util in
    let location = node |> U.member "location" in
    let line = location |> U.member "start" |> U.member "line" |> U.to_int in
    let column_start = location |> U.member "start" |> U.member "column" |> U.to_int in
    let column_end = location |> U.member "end" |> U.member "column" |> U.to_int in
    let length = column_end - column_start in
    (line, column_start + 1, length)

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



(*

module type BaseThing = sig
  type t
  val to_string : t -> string
end

module Make_foo (B : BaseThing) = struct
  type thing = B.t
end

module MyThing = struct
  type t =
    | A of string
    | B of int
  let to_string thing =
    match thing with
    | A str -> str
    | B n -> string_of_int n
end

module FooThing = Make_foo(MyThing)

FooThing.A "test" *)
