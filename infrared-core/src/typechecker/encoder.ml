open Core.Std
open Yojson

(* We don't have exhaustiveness in the NativeEncoder -> InfraredAst conversion. *)
exception Malformed_json_ast of string

(* Exhaustiveness with deferring *)
exception Unimplemented of string

exception Illegal_argument_type_error of string

let working_file = ref "undefined"

module rec NativeEncoder : sig
  include module type of Yojson.Basic
  val parse : string -> Yojson.Basic.json
  val to_string_exn : string -> Yojson.Basic.json -> string
  val to_node_option : Yojson.Basic.json -> Yojson.Basic.json option
end = struct
  include Yojson.Basic
  let parse file = Basic.from_file file

  let to_node_option node =
    let open Yojson.Basic in
    match node with
    | `Null -> None
    | _ -> Some node

  let to_string_exn key node =
    try
      node |> Basic.Util.member key |> Basic.Util.to_string
    with
    | _ ->
      raise
        (Illegal_argument_type_error
           ("`to_string_exn` expects the json node to be a string. \
             Key was '" ^ key ^ "'"))
end

and InfraredEncoder : sig
  val parse_items : fileName:string -> NativeEncoder.json -> InfraredAst.statement list
  val parse_statement : NativeEncoder.json -> InfraredAst.statement list
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

  (* Derive an InfraredAst from a Yojson encoded Shift AST. *)
  let rec parse_items ~(fileName : string) (node : NativeEncoder.json) : InfraredAst.statement list =
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
  val parse_function : NativeEncoder.json -> InfraredAst.statement
  val parse_declaration : NativeEncoder.json -> InfraredAst.statement list
end = struct
  let parse_function (node : NativeEncoder.json) : InfraredAst.statement =
    let module I = InfraredAst in
    I.Skip  (* @TODO *)

  let parse_declaration (node : NativeEncoder.json) : InfraredAst.statement list =
    let module U = NativeEncoder.Util in
    let module EP = ExpressionParser in
    let module IP = IdentifierParser in
    let module I = InfraredAst in
    let kind_str : string =
      node
      |> U.member "declaration"
      |> U.member "kind"
      |> U.to_string
    in
    let declarators : NativeEncoder.json list =
      node
      |> U.member "declaration"
      |> U.member "declarators"
      |> U.to_list
    in
    (* For each delcarator, create a variable declaration statement. *)
    let all_declarations = List.fold_left declarators ~init:[] ~f:(fun acc declarator ->
        let bindingType = declarator
                          |> U.member "binding"
                          |> U.member "type"
                          |> U.to_string in
        let declarations = match bindingType with
          | "ObjectBinding" -> (* produces many declarations *)
            let binding = declarator |> U.member "binding" in
            IP.parse_object_binding kind_str binding
          | "ArrayBinding" -> (* produces many declarations *)
            []
          | "BindingIdentifier" -> (* produces a single declaration *)
            begin
              let binding : InfraredAst.identifier = declarator |> U.member "binding" |> IP.parse_binding_identifier in
              let init : InfraredAst.expression = declarator |> U.member "init" |> EP.parse_expression in
              let kind : InfraredAst.kind = InfraredAst.to_kind kind_str in
              let declaration = I.Declaration (kind, binding, init) in
              [declaration]
            end
          | _ -> []
        in
        declarations @ acc)
    in
    all_declarations
end

and ExpressionParser : sig
  val parse_data_properties : NativeEncoder.json list -> (string * InfraredAst.expression) list
  val parse_property_name : NativeEncoder.json -> string
  val parse_expression : NativeEncoder.json -> InfraredAst.expression
end = struct
  let rec parse_data_properties (nodes : NativeEncoder.json list) : (string * InfraredAst.expression) list =
    let module U = NativeEncoder.Util in
    let module N = NativeEncoder in
    let module I = InfraredAst in
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
    let module I = InfraredAst in
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

  and parse_expression (node : NativeEncoder.json) : InfraredAst.expression =
    let module U = NativeEncoder.Util in
    let module N = NativeEncoder in
    let module I = InfraredAst in
    let node_opt = node |> N.to_node_option in
    match node_opt with
    | None ->
      (* An expression that's ever `null` in our raw ast should imply the Null expression. *)
      I.Null
    | Some node ->
      begin
        let t = N.to_string_exn "type" node in
        match t with
        | "LiteralNumericExpression" ->
          let value = node |> U.member "value" |> U.to_int in
          I.Number value
        | "LiteralStringExpression" ->
          let value = node |> U.member "value" |> U.to_string in
          I.String value
        | "LiteralNullExpression" ->
          I.Null
        | "ObjectExpression" ->
          let properties = node |> U.member "properties" |> U.to_list in
          let data_properties = parse_data_properties properties in
          (I.Object data_properties)
        | _ ->
          let reason = Printf.sprintf "ExpressionParser.parse_expression: %s" t in
          let (line, column, length) = Utils.destructure node in
          Printf.printf "%d %d %d" line column length;
          let err = Error_handler.exposed_error
              ~source:(!working_file)
              ~loc_line:line
              ~loc_column:column
              ~loc_length:length
              ~msg:reason
              ~reason:reason in
          raise (Unimplemented err)
      end
end

and IdentifierParser : sig
  val parse_object_binding : string -> NativeEncoder.json -> InfraredAst.statement list
  val parse_binding_identifier : NativeEncoder.json -> InfraredAst.identifier
end = struct
  let parse_binding_identifier (node : NativeEncoder.json) : InfraredAst.identifier =
    let module U = NativeEncoder.Util in
    let module N = NativeEncoder in
    let module I = InfraredAst in
    let name = node |> U.member "name" |> U.to_string in
    I.(name)

  let parse_object_binding (kind : string) (node : NativeEncoder.json) : InfraredAst.statement list =
    let module U = NativeEncoder.Util in
    let module N = NativeEncoder in
    let module EP = ExpressionParser in
    let module I = InfraredAst in
    (* These kinds of declarations are super interesting for us. The way we
     * basically want to handle this is kind of rewriting what's really going
     * on behind the syntactic sugar.
     *
     * <KIND> {foo, bar = <E2>} = <E>
     *
     *      ↓      ↓      ↓
     *
     * <KIND> foo = <E>.foo
     * <KIND> bar = <E>.bar || <E2>
     *
     *
     *
     * <KIND> {foo: bar} = <E>
     * <KIND> {foo} = <E>
     *
     *  ↓      ↓      ↓
     *
     * <KIND> bar = <E>.foo
     * <KIND> foo = <E>.foo
     *
     * For reference,
     * <KIND> foo = <E>.foo
     *         ^         ^
     *   identifier     property
     *
    *)
    let kind_t = I.to_kind kind in
    let properties = node |> U.member "properties" |> U.to_list in
    let declarations = List.fold_left properties ~init:[] ~f:(fun acc property ->
        let propertyType = N.to_string_exn "type" property in
        let (identifier_str, property_str) = match propertyType with
          | "BindingPropertyIdentifier" ->
            let identifier_str = property
                                 |> U.member "binding"
                                 |> U.member "name"
                                 |> U.to_string
            in
            let property_str = identifier_str in
            (identifier_str, property_str)
          | _ -> raise (Malformed_json_ast "IdentifierParser.parse_object_binding declarations")
        in
        acc)
    (* let properties_t = properties |> N.to_string_exn "type" in
       match properties_t with
       | "BindingPropertyIdentifier" -> raise (Unimplemented "BindingPropertyIdentifier")
       | "BindingPropertyProperty" -> raise (Unimplemented "BindingPropertyProperty")
       | _ -> raise (Unimplemented "Properties") *)
    in
    []
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
    (* Lowest length we'll allow is 1. There are cases where length can be negative
     * here when the expression is multiple lines long. *)
    let length = max (column_end - column_start) 1 in
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

    let _ = MyFoo.A "";;
    # Error: Unbound constructor MyFoo.A

FooThing.A "test" *)
