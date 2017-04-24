
(* Dope trick for defining recursive modules
 * https://blogs.janestreet.com/a-trick-recursive-modules-from-recursive-signatures/
 *
 * This AST implementation is based off of the Shift AST specification
 * described here: http://shift-ast.org/ *)

module Token = Lexer.Token
module Lex_env = Lexer.Lex_env

(* Supporting types *)
(*
module rec Arguments : sig
  type t = 
    | SpreadElement of SpreadElement.t
    | Expression of Expression.t
end = Arguments

and Identifier : sig
  type t = Identifier of string
end = Identifier

and IdentifierName : sig
  type t = IdentifierName of string
end = IdentifierName

and Label : sig
  type t = Label of string
end = Label

and VariableDeclarationKind : sig
  type t = 
    | Var
    | Let
    | Const 
end = VariableDeclarationKind

and CompoundAssignmentOperator : sig 
  type t = 
    | Plus                (*    +=    *)
    | Minus               (*    -=    *)
    | Mult                (*    *=    *)
    | Div                 (*    /=    *)
    | Mod                 (*    %=    *)
    | Pow                 (*    **=   *)
    | LeftShift           (*    <<=   *)
    | RightShift          (*    >>=   *)
    | RightShiftUnsigned  (*    >>>=  *)
    | Or                  (*    |=    *)
    | XOr                 (*    ^=    *)
    | And                 (*    &=    *)
end = CompoundAssignmentOperator

and BinaryOperator : sig 
  type t = 
    | Equal               (*    ==    *)
    | NotEqual            (*    !=    *)
    | StrictEqual         (*    ===   *)
    | StrictNotEqual      (*    !==   *)
    | LessThan            (*    <     *)
    | LessThanEqual       (*    <=    *)
    | GreaterThan         (*    >     *)
    | GreaterThanEqual    (*    >=    *)
    | In                 
    | InstanceOf          
    | LeftShift           (*    <<    *)
    | RightShift          (*    >>    *)
    | RightShiftUnsigned  (*    >>>   *)
    | Plus                (*    +     *)
    | Minus               (*    -     *)
    | Mult                (*    *     *)
    | Div                 (*    /     *)
    | Mod                 (*    %     *)
    | Pow                 (*    **    *)
    | Comma               (*    ,     *)
    | LogicalOr           (*    ||    *)
    | LogicalAnd          (*    &&    *)
    | Or                  (*    |     *)
    | Xor                 (*    ^     *)
    | And                 (*    &     *)
end = BinaryOperator

and UnaryOperator : sig
  type t = 
    | Plus                (*    +     *)
    | Minus               (*    -     *)
    | Bang                (*    !     *)
    | Not                 (*    ~     *)
    | Typeof              
    | Void                
    | Delete              
end = UnaryOperator

and UpdateOperator : sig
  type t = 
    | Increment           (*    ++    *)
    | Decrement           (*    --    *)
end = UpdateOperator

and Function : sig
  type t = {
    isGenerator: bool;
    params: FormalParameters.t;
    body: FunctionBody.t;
  }
end = Function

(* Node classes *)

and Node : sig
  type t = Identifier.t * t'

  and t' = 
    | Program of Program.t
    | Statement of Statement.t
    | Expression of Expression.t
    | PropertyName of PropertyName.t 
    | ImportDeclaration of ImportDeclaration.t
    | ExportDeclaration of ExportDeclaration.t
    | VariableReference  of VariableReference.t
    | BindingWithDefault of BindingWithDefault.t
end = Node

and Program : sig
  type t =
    | Script of Script.t
end = Program

and Statement : sig
  type t = 
    | IterationStatement of IterationStatement.t
    | Empty
end = Statement

and IterationStatement : sig
  type t = {
    body:  Statement.t;
  }
end = IterationStatement

and Expression : sig
  type t = 
    | MemberExpression of MemberExpression.t
end = Expression

and MemberExpression : sig
  type t = 
    | Object_E of Expression.t
    | Object_S of Super.t
end = MemberExpression

and PropertyName : sig
  type t
end = PropertyName

and NamedObjectProperty : sig
  type t = {
    name: PropertyName.t;
  }
end = NamedObjectProperty

and MethodDefinition : sig
  type t = {
    body: FunctionBody.t;
  }
end = MethodDefinition

and ImportDeclaration : sig
  type t = {
    moduleSpecifier: string;
  }
end = ImportDeclaration

and ExportDeclaration : sig
  type t
end = ExportDeclaration

and VariableReference : sig
  type t = {
    name: Identifier.t;
  }
end = VariableReference

(* Bindings *)

and BindingPattern : sig
  type t = 
    | ObjectBinding of ObjectBinding.t
    | ArrayBinding of ArrayBinding.t
end = BindingPattern

and Binding : sig
  type t =
    | BindingPattern of BindingPattern.t
    | BindingIdentifier of BindingIdentifier.t
end = Binding

and SimpleAssignmentTarget : sig
  type t = 
    | AssignmentTargetIdentifier of AssignmentTargetIdentifier.t
    | MemberAssignmentTarget of MemberAssignmentTarget.t
end = SimpleAssignmentTarget

and AssignmentTargetPattern : sig
  type t = 
    | ObjectAssignmentTarget of ObjectAssignmentTarget.t
    | ArrayAssignmentTarget of ArrayAssignmentTarget.t
end = SimpleAssignmentTarget

and AssignmentTarget : sig
  type t = 
    | AssignmentTargetPattern of AssignmentTargetPattern.t
    | SimpleAssignmentTarget of SimpleAssignmentTarget.t
end = AssignmentTarget 

and Parameter : sig
  type t = 
    | Binding of Binding.t
    | BindingWithDefault of BindingWithDefault.t
end = Parameter

and BindingWithDefault : sig
  type t = {
    binding: Binding.t;
    init: Expression.t;
  }
end = BindingWithDefault

and BindingIdentifier : sig
  type t
end = BindingIdentifier 

*)
