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

let rec string_of_ast (ast : Loc.t Ast.program * (Loc.t * Err.t) list) : string =
  let ((_loc, stmts, _comments), _err) = ast in
  let stmts' = List.map (fun stmt -> string_of_statement stmt) stmts in
  String.concat "\n" stmts'

and string_of_statement stmt : string =
  let open S in
  let (_loc, statement) = stmt in
  match statement with
  | Block _ -> "Block"
  | Break _ -> "Break"
  | ClassDeclaration _ -> "ClassDeclaration"
  | Continue _ -> "Continue"
  | Debugger -> "Debugger"
  | DeclareClass _ -> "DeclareClass"
  | DeclareExportDeclaration _ -> "DeclareExportDeclaration"
  | DeclareFunction _ -> "DeclareFunction"
  | DeclareInterface _ -> "DeclareInterface"
  | DeclareModule _ -> "DeclareModule"
  | DeclareModuleExports _ -> "DeclareModuleExports"
  | DeclareTypeAlias _ -> "DeclareTypeAlias"
  | DeclareOpaqueType _ -> "DeclareOpaqueType"
  | DeclareVariable _ -> "DeclareVariable"
  | DoWhile _ -> "DoWhile"
  | Empty -> "Empty"
  | ExportDefaultDeclaration _ -> "ExportDefaultDeclaration"
  | ExportNamedDeclaration _ -> "ExportNamedDeclaration"
  | Expression _ -> "Expression"
  | For _ -> "For"
  | ForIn _ -> "ForIn"
  | ForOf _ -> "ForOf"
  | FunctionDeclaration _ -> "FunctionDeclaration"
  | If _ -> "If"
  | ImportDeclaration _ -> "ImportDeclaration"
  | InterfaceDeclaration _ -> "InterfaceDeclaration"
  | Labeled _ -> "Labeled"
  | Return _ -> "Return"
  | Switch _ -> "Switch"
  | Throw _ -> "Throw"
  | Try _ -> "Try"
  | TypeAlias _ -> "TypeAlias"
  | OpaqueType _ -> "OpaqueType"
  | VariableDeclaration obj ->
    Printf.sprintf "(VariableDeclaration) %s"
      (string_of_kind obj.kind)
  | While _ -> "While"
  | With _ -> "With"

and string_of_kind kind : string =
  let open S.VariableDeclaration in
  match kind with
  | Var -> "Var"
  | Let -> "Let"
  | Const -> "Const"
