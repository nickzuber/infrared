open InfraredUtils
module Ast = Flow_parser.Ast
module Loc = Flow_parser.Loc
module Err = Flow_parser.Parse_error
module E = Ast.Expression
module P = Ast.Pattern
module S = Ast.Statement
module L = Ast.Literal
module SL = Ast.StringLiteral
module T = Ast.Type
module V = Ast.Variance
module C = Ast.Class
module F = Ast.Function
module J = Ast.JSX

let todo (str : string) : string =
  Printf.sprintf "\x1b[33m(@TODO %s)\x1b[39m" str

let format_sexp (sexp : string) : string =
  let offset = 2 in
  let depth = ref 0 in
  let chars = Utils.char_list_of_string sexp in
  let formatted_chars_as_strings = List.map
      (fun char ->
         match char with
         | '(' ->
           let formatted_spacing = String.make (!depth * offset) ' ' in
           depth := !depth + 1;
           "\n" ^ formatted_spacing ^ (String.make 1 char)
         | ')' ->
           depth := !depth - 1;
           String.make 1 char
         | _ -> String.make 1 char
      )
      chars
  in
  String.concat "" formatted_chars_as_strings

(** Assumes that the first element of the tuple is location data. *)
let strip_location e =
  let (_loc, e') = e in
  e'

(** Creates a version of a stringify function that accepts a list of its items. *)
let listify fn items : string =
  if List.length items = 0
  then "[]"
  else
    let items' = List.map (fun item -> fn item) items in
    let items'' = String.concat " " items' in
    Printf.sprintf "[%s]" items''

(** Formats a Flow_ast into a formatted S-Expression. *)
let rec string_of_ast (ast : Loc.t Ast.program * (Loc.t * Err.t) list) : string =
  let ((_loc, stmts, _comments), _err) = ast in
  let stmts' = List.map (fun stmt -> string_of_statement stmt) stmts in
  let sexp = String.concat "\n" stmts' in
  format_sexp sexp

and string_of_statement stmt : string =
  let open S in
  let (_loc, statement) = stmt in
  match statement with
  | Block _ -> (todo "Block")
  | Break _ -> (todo "Break")
  | ClassDeclaration obj ->
    let string_of_expressions = listify string_of_expression in
    Printf.sprintf "(ClassDeclaration (id: %s) (decorators: %s) (body: %s))"
      (string_of_identifier_maybe obj.id)
      (string_of_expressions obj.classDecorators)
      (string_of_body obj.body)
  | Continue _ -> (todo "Continue")
  | Debugger -> (todo "Debugger")
  | DeclareClass _ -> (todo "DeclareClass")
  | DeclareExportDeclaration _ -> (todo "DeclareExportDeclaration")
  | DeclareFunction _ -> (todo "DeclareFunction")
  | DeclareInterface _ -> (todo "DeclareInterface")
  | DeclareModule _ -> (todo "DeclareModule")
  | DeclareModuleExports _ -> (todo "DeclareModuleExports")
  | DeclareTypeAlias _ -> (todo "DeclareTypeAlias")
  | DeclareOpaqueType _ -> (todo "DeclareOpaqueType")
  | DeclareVariable _ -> (todo "DeclareVariable")
  | DoWhile _ -> (todo "DoWhile")
  | Empty -> (todo "Empty")
  | ExportDefaultDeclaration _ -> (todo "ExportDefaultDeclaration")
  | ExportNamedDeclaration _ -> (todo "ExportNamedDeclaration")
  | Expression obj ->
    Printf.sprintf "(Expression %s)"
      (string_of_expression obj.expression)
  | For _ -> (todo "For")
  | ForIn _ -> (todo "ForIn")
  | ForOf _ -> (todo "ForOf")
  | FunctionDeclaration fn ->
    Printf.sprintf "(FunctionDeclaration %s)"
      (string_of_function fn)
  | If _ -> (todo "If")
  | ImportDeclaration obj ->
    Printf.sprintf "(ImportDeclaration %s, (default: %s), (source: \"%s\"))"
      (string_of_specifier obj.specifiers)
      (string_of_identifier_maybe obj.default)
      (string_of_stringliteral obj.source)
  | InterfaceDeclaration _ -> (todo "InterfaceDeclaration")
  | Labeled _ -> (todo "Labeled")
  | Return obj ->
    Printf.sprintf "(Return %s)"
      (string_of_expression_maybe obj.argument)
  | Switch _ -> (todo "Switch")
  | Throw _ -> (todo "Throw")
  | Try _ -> (todo "Try")
  | TypeAlias _ -> (todo "TypeAlias")
  | OpaqueType _ -> (todo "OpaqueType")
  | VariableDeclaration obj ->
    let string_of_declarations = listify string_of_declaration in
    Printf.sprintf "(VariableDeclaration %s, %s)"
      (string_of_kind obj.kind)
      (string_of_declarations obj.declarations)
  | While _ -> (todo "While")
  | With _ -> (todo "With")

and string_of_expression expr : string =
  let open E in
  let (_loc, expression) = expr in
  match expression with
  | Array _ -> (todo "Array")
  | ArrowFunction fn ->
    Printf.sprintf "(ArrowFunction %s)"
      (string_of_function fn)
  | Assignment obj ->
    let open E.Assignment in
    let left = string_of_pattern obj.left in
    let right = string_of_expression obj.right in
    let operator = string_of_assignment_op obj.operator in
    Printf.sprintf "(Assignment %s, %s, %s)"
      operator left right
  | Binary obj ->
    let open E.Binary in
    let left = string_of_expression obj.left in
    let right = string_of_expression obj.right in
    let operator = string_of_binary_op obj.operator in
    Printf.sprintf "(Binary %s, %s, %s)"
      operator left right
  | Call obj ->
    let arguments_list = List.map
        (string_of_expression_or_spread)
        obj.arguments in
    let arguments = String.concat ", " arguments_list in
    Printf.sprintf "(Call %s, %s)"
      (string_of_expression obj.callee)
      arguments
  | Class _ -> (todo "Class")
  | Comprehension _ -> (todo "Comprehension")
  | Conditional _ -> (todo "Conditional")
  | Function fn ->
    Printf.sprintf "(Function %s)"
      (string_of_function fn)
  | Generator _ -> (todo "Generator")
  | Identifier obj -> string_of_identifier obj
  | Import _ -> (todo "Import")
  | JSXElement _ -> (todo "JSXElement")
  | JSXFragment _ -> (todo "JSXFragment")
  | Literal obj ->
    Printf.sprintf "(Literal %s)"
      (string_of_literal obj)
  | Logical _ -> (todo "Logical")
  | Member obj ->
    Printf.sprintf "(Member %s)"
      (string_of_member_property obj.property)
  | MetaProperty _ -> (todo "MetaProperty")
  | New _ -> (todo "New")
  | Object obj ->
    let string_of_object_properties = listify string_of_object_property in
    Printf.sprintf "(Object: %s)"
      (string_of_object_properties obj.properties)
  | Sequence _ -> (todo "Sequence")
  | Super -> (todo "Super")
  | TaggedTemplate _ -> (todo "TaggedTemplate")
  | TemplateLiteral _ -> (todo "TemplateLiteral")
  | This -> (todo "This")
  | TypeCast _ -> (todo "TypeCast")
  | Unary obj ->
    Printf.sprintf "(Unary %s, %s)"
      (string_of_unary_op obj.operator)
      (string_of_expression obj.argument)
  | Update _ -> (todo "Update")
  | Yield _ -> (todo "Yield")

and string_of_kind kind : string =
  let open S.VariableDeclaration in
  match kind with
  | Var -> "Var"
  | Let -> "Let"
  | Const -> "Const"

and string_of_specifier specifier_maybe : string =
  match specifier_maybe with
  | Some (ImportNamedSpecifiers obj_list) ->
    (List.map (fun obj ->
         let open S.ImportDeclaration in
         Printf.sprintf "(local: %s), (remote: %s)"
           (string_of_identifier_maybe obj.local)
           (string_of_identifier obj.remote)
       ) obj_list)
    |> String.concat ", "
  | Some (ImportNamespaceSpecifier identifier) ->
    let identifier' = identifier
                      |> strip_location
                      |> string_of_identifier
    in
    Printf.sprintf "(specifier: %s)"
      identifier'
  | None -> "(specifier: ∅)"

and string_of_declaration obj : string =
  let open S.VariableDeclaration.Declarator in
  let obj' = strip_location obj in
  Printf.sprintf "(Declarator %s, %s)"
    (Printf.sprintf "(id: %s)" (string_of_pattern obj'.id))
    (Printf.sprintf "(init: %s)" (string_of_expression_maybe obj'.init))

and string_of_pattern pattern : string =
  let (_loc, pattern) = pattern in
  let open P in
  let pattern' = match pattern with
    | Object _ -> (todo "Pattern->Object")
    | Array _ -> (todo "Pattern->Array")
    | Assignment obj -> string_of_pattern_assignment obj
    (* Expression.Identifier is a subset of Pattern.Identifier,
     * and since we only case about the common data we can actually
     * get away with just reusing that function here since OCaml
     * is structurally typed. *)
    | Identifier id -> string_of_identifier id.name
    (* Same as Expression so we can get away with reusing this. *)
    | Expression expr -> string_of_expression expr
  in
  Printf.sprintf "(Pattern %s)" pattern'

and string_of_pattern_assignment obj : string =
  let open P.Assignment in
  let left = string_of_pattern obj.left in
  let right = string_of_expression obj.right in
  Printf.sprintf "(Assignment %s, %s)"
    left right

and string_of_unary_op op : string =
  let open E.Unary in
  match op with
  | Minus -> "MINUS"
  | Plus -> "PLUS"
  | Not -> "NOT"
  | BitNot -> "BITNOT"
  | Typeof -> "TYPEOF"
  | Void -> "VOID"
  | Delete -> "DELETE"
  | Await -> "AWAIT"

and string_of_binary_op op : string =
  let open E.Binary in
  match op with
  | Equal -> "EQUAL"
  | NotEqual -> "NOTEQUAL"
  | StrictEqual -> "STRICTEQUAL"
  | StrictNotEqual -> "STRICTNOTEQUAL"
  | LessThan -> "LESSTHAN"
  | LessThanEqual -> "LESSTHANEQUAL"
  | GreaterThan -> "GREATERTHAN"
  | GreaterThanEqual -> "GREATERTHANEQUAL"
  | LShift -> "LSHIFT"
  | RShift -> "RSHIFT"
  | RShift3 -> "RSHIFT3"
  | Plus -> "PLUS"
  | Minus -> "MINUS"
  | Mult -> "MULT"
  | Exp -> "EXP"
  | Div -> "DIV"
  | Mod -> "MOD"
  | BitOr -> "BITOR"
  | Xor -> "XOR"
  | BitAnd -> "BITAND"
  | In -> "IN"
  | Instanceof -> "INSTANCEOF"

and string_of_assignment_op op : string =
  let open E.Assignment in
  match op with
  | Assign -> "ASSIGN"
  | PlusAssign -> "PLUSASSIGN"
  | MinusAssign -> "MINUSASSIGN"
  | MultAssign -> "MULTASSIGN"
  | ExpAssign -> "EXPASSIGN"
  | DivAssign -> "DIVASSIGN"
  | ModAssign -> "MODASSIGN"
  | LShiftAssign -> "LSHIFTASSIGN"
  | RShiftAssign -> "RSHIFTASSIGN"
  | RShift3Assign -> "RSHIFT3ASSIGN"
  | BitOrAssign -> "BITORASSIGN"
  | BitXorAssign -> "BITXORASSIGN"
  | BitAndAssign -> "BITANDASSIGN"

and string_of_expression_or_spread expr_or_spread : string =
  let open E in
  match expr_or_spread with
  | Expression expr -> string_of_expression expr
  | Spread spread ->
    let obj = strip_location spread in
    "..." ^ (string_of_expression obj.argument)

and string_of_stringliteral str : string =
  let name = strip_location str in
  name.value

and string_of_member_property prop : string =
  let open E.Member in
  match prop with
  | PropertyIdentifier id -> string_of_identifier id
  | PropertyPrivateName _name -> (todo "PropertyPrivateName")
  | PropertyExpression expr -> string_of_expression expr

and string_of_literal obj : string =
  match obj.value with
  | String str -> Printf.sprintf "\"%s\"" str
  | Boolean true -> "TRUE"
  | Boolean false -> "FALSE"
  | Null -> "NULL"
  | Number n -> string_of_float n
  | RegExp obj -> Printf.sprintf "/%s/" obj.pattern

and string_of_expression_maybe expression_maybe : string =
  match expression_maybe with
  | Some expr -> string_of_expression expr
  | None -> "∅"

and string_of_identifier_maybe identifier_maybe : string =
  match identifier_maybe with
  | Some i -> string_of_identifier i
  | None -> "∅"

and string_of_identifier identifier : string =
  let name = strip_location identifier in
  Printf.sprintf "(Identifier \"%s\")" name

and string_of_body obj : string =
  let body = strip_location obj in
  let string_of_bodies = listify string_of_body_element in
  string_of_bodies body.body

and string_of_body_element elem : string =
  let open C in
  match elem with
  | Body.Method mthd ->
    let obj = strip_location mthd in
    let static =
      Printf.sprintf "(static: %s)"
        (string_of_bool obj.static)
    in
    let kind = string_of_method_kind obj.kind in
    let key = string_of_object_key obj.key in
    let value = obj.value
                |> strip_location
                |> string_of_function
    in
    Printf.sprintf "(Method %s, %s, %s, %s)"
      static
      kind
      (Printf.sprintf "(key: %s)" key)
      value
  | Body.Property obj ->
    let obj = strip_location obj in
    let key = string_of_object_key obj.key in
    let value = string_of_expression_maybe obj.value in
    Printf.sprintf "(Property %s, %s, %s)"
      (Printf.sprintf "(static: %s)" (string_of_bool obj.static))
      (Printf.sprintf "(key: %s)" key)
      (Printf.sprintf "(value: %s)" value)
  | Body.PrivateField _ -> (todo "private field")

and string_of_method_kind kind : string =
  let open C.Method in
  let kind' = match kind with
    | Constructor -> "CONSTRUCTOR"
    | Method -> "METHOD"
    | Get -> "GET"
    | Set -> "SET"
  in
  Printf.sprintf "(kind: %s)" kind'

and string_of_object_key key : string =
  let open E.Object.Property in
  match key with
  | Literal lit -> lit
                   |> strip_location
                   |> string_of_literal
  | Identifier id -> string_of_identifier id
  | PrivateName _ -> (todo "PrivateName")
  | Computed _ -> (todo "Computed")

and string_of_function fn : string =
  let id = string_of_identifier_maybe fn.id in
  let body = string_of_function_body fn.body in
  Printf.sprintf "(function %s, %s)"
    (Printf.sprintf "(id: %s)" id)
    (Printf.sprintf "(body: %s)" body)

and string_of_function_body body : string =
  match body with
  | BodyExpression expr ->
    Printf.sprintf "(BodyExpression %s)"
      (string_of_expression expr)
  | BodyBlock block ->
    let (_loc, block) = block in
    Printf.sprintf "(BodyBlock %s)"
      (string_of_block block)

and string_of_block block : string =
  let string_of_statements = listify string_of_statement in
  string_of_statements block.body

and string_of_object_property prop : string =
  let open E.Object in
  match prop with
  | Property _ -> (todo "Expression->Object->Property")
  | SpreadProperty _ -> (todo "Expression->Object->SpreadProperty")
