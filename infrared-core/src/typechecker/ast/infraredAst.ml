open Core.Std

exception Illegal_kind of string

(* Type constraints. *)
type t =
  | T_STRING
  | T_NUMBER
  | T_BOOL
  | T_OBJECTABLE of (string * t) list
  | T_NULLABLE
  | T_CALLABLE

(* Valid primitives in javascript. These are generally used when
 * referencing the Predicate check or some kind of native type refining.
 * These are not types, but rather describe structures of expressions that
 * will eventually be typed. VariableDeclaration*)
type primitive =
  | P_string
  | P_number
  | P_boolean
  | P_null
  | P_undefined
  | P_object of (string * expression) list

and identifier = string

and kind = | Var | Let | Const

and predicate =
  | Typeof of expression * string

and expression =
  | String of string
  | Number of int
  | Boolean of bool
  | Null
  | Undefined
  | Object of (string * expression) list
  | Predicate of predicate
  | Not of expression
  | Function of identifier list * expression list
  | Or of expression * expression
  | And of expression * expression
  | Call of expression * expression
  | Assignment of identifier * expression
  | Identifier of identifier
  | Access of expression * identifier

(*

var x = obj.foo.bar.baz[0]
        |-------------||-|
        |---------| |-|
        |-----| |-|
        |-| |-|


id := string

*)

and statement =
  | Import of (identifier * string) list
  | Export of identifier list
  | Skip
  | Declaration of kind * identifier * expression
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
  | Declaration (k, i, e) ->
    Printf.sprintf "Declaration <%s> %s = %s"
      (string_of_kind k)
      (string_of_identifier i)
      (string_of_expression e)
  | _ -> "Other"

and string_of_expression (expression : expression) : string =
  match expression with
  | Identifier i ->
    Printf.sprintf "Identifier(%s)"
      (string_of_identifier i)
  | Predicate e ->
    Printf.sprintf "Predicate(%s)"
      (string_of_predicate e)
  | Not e ->
    Printf.sprintf "!%s"
      (string_of_expression e)
  | Or (e1, e2) ->
    Printf.sprintf "%s || %s"
      (string_of_expression e1)
      (string_of_expression e2)
  | And (e1, e2) ->
    Printf.sprintf "%s && %s"
      (string_of_expression e1)
      (string_of_expression e2)
  | Call (e1, e2) ->
    Printf.sprintf "Call(%s, %s)"
      (string_of_expression e1)
      (string_of_expression e2)
  | Assignment (i, e) ->
    Printf.sprintf "Assignment(%s, %s)"
      (string_of_identifier i)
      (string_of_expression e)
  | Function (i_lst, e_lst) -> "@TODO(func)"
  | Access (e, i) ->
    Printf.sprintf "Access(%s, %s)"
      (string_of_expression e)
      (string_of_identifier i)
  | String s ->
    Printf.sprintf "String(\"%s\")" s
  | Number n ->
    Printf.sprintf "Number(%n)" n
  | Boolean b ->
    Printf.sprintf "Boolean(%s)"
      (string_of_bool b)
  | Null -> "Null"
  | Undefined -> "Undefined"
  | Object members ->
    Printf.sprintf "Object {%s }"
      (List.fold_left members ~init:"" ~f:(fun acc member ->
           let (key, expr) = member in
           let field = Printf.sprintf "%s: %s"
               key (string_of_expression expr)
           in
           acc ^ " " ^ field ^ ","))

and string_of_predicate (predicate : predicate) : string =
  match predicate with
  | Typeof (e, str) ->
    Printf.sprintf "%s === %s"
      (string_of_expression e)
      str

and to_kind (kind : string) : kind =
  match kind with
  | "var" -> Var
  | "let" -> Let
  | "const" -> Const
  | _ -> raise (Illegal_kind kind)

and string_of_kind kind =
  match kind with
  | Var -> "VAR"
  | Let -> "LET"
  | Const -> "CONST"

and string_of_identifier (identifier : identifier) : string =
  Printf.sprintf "%s" identifier

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
