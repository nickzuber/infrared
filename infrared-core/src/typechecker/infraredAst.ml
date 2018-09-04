open Core.Std

(* Type constraints. *)
type t =
  | T_STRING
  | T_NUMBER
  | T_BOOL
  | T_OBJECTABLE of (string * t) list
  | T_NULLABLE
  | T_CALLABLE

(* Valid primitives in javascript. These are generally used when
 * referencing the Predicate check or some kind of native type refining. *)
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

and identifier =
  | Identifer of string
  | Member of identifier * identifier

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
      (string_of_identifier i)
      (string_of_expression e)
  | _ -> "Other"

and string_of_identifier (identifier : identifier) : string =
  match identifier with
  | Identifer str -> str
  | Member (obj, field) ->
    Printf.sprintf "%s.%s"
      (string_of_identifier obj)
      (string_of_identifier field)

and string_of_expression (expression : expression) : string =
  match expression with
  | Primitive p ->
    Printf.sprintf "Primitive %s"
      (string_of_primative p)
  | Variable i ->
    Printf.sprintf "Variable %s"
      (string_of_identifier i)
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
      (string_of_identifier i)
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
  | P_object -> "OBJECT"

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
                  \x1b[1mExportss\x1b[0m\n%s\
                  \x1b[1mStatements\x1b[0m\n%s"
    (string_of_imports i)
    (string_of_exports e)
    (string_of_statements s)
