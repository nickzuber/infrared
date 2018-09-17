open Core.Std

(* Type constraints. *)
type t =
  | T_STRING
  | T_NUMBER
  | T_BOOL
  | T_OBJECTABLE of (string * t) list
  | T_NULLABLE
  | T_CALLABLE

(* Structual requirements for an Identifier module type. *)
module type IdentifierBase = sig
  type t
  val to_string : t -> string
end

(* Functor that creates an Infrared AST *)
module Make_InfraredAst (I : IdentifierBase) = struct
  type identifier = I.t

  (* Valid primitives in javascript. These are generally used when
   * referencing the Predicate check or some kind of native type refining.
   * These are not types, but rather describe structures of expressions that
   * will eventually be typed. *)
  type primitive =
    | P_string
    | P_number
    | P_boolean
    | P_null
    | P_undefined
    | P_object of (string * expression) list

  and predicate =
    | Typeof of expression * string

  and expression =
    | Primitive of primitive
    | Variable of identifier
    | Predicate of predicate
    | Not of expression
    | Function of identifier list * expression list
    | Or of expression * expression
    | And of expression * expression
    | Call of expression * expression
    | Assignment of identifier * expression

  and statement =
    | Import of (identifier * string) list
    | Export of identifier list
    | Skip
    | Declaration of identifier * expression
    | Expression of expression
    | If of expression * expression * expression
    | While of expression * expression * expression
    | For of expression * expression * expression

  type env = (identifier, t) Hashtbl.t

  type mutation = (identifier, t) Hashtbl.t

  type program = {
    (* Includes all import mappings from identifiers to location.
     * Supports imports for CommonJS and ES6. *)
    imports: (identifier * string) list;

    (* Includes all import mappings from identifiers to location.
     * Supports imports for CommonJS and ES6. *)
    exports: identifier list;

    (* Collection of all parsed statements. *)
    statements: statement list;
  }

  let rec string_of_statement (statement : statement) : string =
    match statement with
    | Skip -> "Skip"
    | Declaration (i, e) ->
      Printf.sprintf "Declaration %s, %s"
        (I.to_string i)
        (string_of_expression e)
    | _ -> "Other"

  and string_of_expression (expression : expression) : string =
    match expression with
    | Primitive p ->
      Printf.sprintf "Primitive %s"
        (string_of_primative p)
    | Variable i ->
      Printf.sprintf "Variable %s"
        (I.to_string i)
    | Predicate e ->
      Printf.sprintf "Predicate %s"
        (string_of_predicate e)
    | Not e ->
      Printf.sprintf "Not %s"
        (string_of_expression e)
    | Or (e1, e2) ->
      Printf.sprintf "Or %s, %s"
        (string_of_expression e1)
        (string_of_expression e2)
    | And (e1, e2) ->
      Printf.sprintf "And %s, %s"
        (string_of_expression e1)
        (string_of_expression e2)
    | Call (e1, e2) ->
      Printf.sprintf "Call %s, %s"
        (string_of_expression e1)
        (string_of_expression e2)
    | Assignment (i, e) ->
      Printf.sprintf "Assignment %s, %s"
        (I.to_string i)
        (string_of_expression e)
    | Function (i_lst, e_lst) -> "__func__"

  and string_of_predicate (predicate : predicate) : string =
    match predicate with
    | Typeof (e, str) ->
      Printf.sprintf "%s === %s"
        (string_of_expression e)
        str

  and string_of_primative (primitive : primitive) : string =
    match primitive with
    | P_string -> "STRING"
    | P_number -> "NUMBER"
    | P_boolean -> "BOOLEAN"
    | P_null -> "NULL"
    | P_undefined -> "UNDEFINED"
    | P_object members ->
      Printf.sprintf "OBJECT {%s }"
        (List.fold_left members ~init:"" ~f:(fun acc member ->
             let (key, expr) = member in
             let field = Printf.sprintf "%s: %s"
                 key (string_of_expression expr)
             in
             acc ^ " " ^ field ^ ","))

  let string_of_imports (imports : (identifier * string) list) : string =
    ""

  let string_of_exports (exports : identifier list) : string =
    ""

  let string_of_statements (statements : statement list) : string =
    List.fold_left statements ~init:"" ~f:(fun acc stmt ->
        Printf.sprintf "%s%s\n" acc (string_of_statement stmt))

  let string_of_ast (ast : program) : string =
    let { imports = i
        ; exports = e
        ; statements = s } = ast in
    Printf.sprintf "\x1b[1mImports\x1b[0m\n%s\
                    \x1b[1mExports\x1b[0m\n%s\
                    \x1b[1mStatements\x1b[0m\n%s"
      (string_of_imports i)
      (string_of_exports e)
      (string_of_statements s)
end

module StandardIdentifier = struct
  type t =
    | Identifier of string
    | Member of t * t

  let rec to_string (identifier : t) : string =
    match identifier with
    | Identifier str -> str
    | Member (obj, field) ->
      Printf.sprintf "%s.%s"
        (to_string obj)
        (to_string field)
end

module ScopedIdentifier = struct
  type t = scope * identifier

  and identifier =
    | Identifier of string
    | Member of identifier * identifier

  and scope = int

  let rec to_string (scoped_identifier : t) : string =
    let (scope, identifier) = scoped_identifier in
    Printf.sprintf "%s {%d}"
      (string_of_identifier identifier)
      scope

  and string_of_identifier (identifier : identifier) : string =
    match identifier with
    | Identifier str -> str
    | Member (obj, field) ->
      Printf.sprintf "%s.%s"
        (string_of_identifier obj)
        (string_of_identifier field)
end

module StandardInfraredAst = Make_InfraredAst(StandardIdentifier)
module ScopedInfraredAst = Make_InfraredAst(ScopedIdentifier)
