
(* Dope trick for defining recursive modules
 * https://blogs.janestreet.com/a-trick-recursive-modules-from-recursive-signatures/
 *
 * This AST implementation is based off of the Shift AST specification
 * described here: http://shift-ast.org/ 
 *
 * Note for translating spec.idl to ocaml:
 * ```
 * interface B : A { }
 * ---
 * module rec A : sig
 *   type t = PropA * t'
 *   type t' =
 *     | B of B.t
 * end = A
 * 
 * and B : sig 
 *   type t = PropA * t'
 *   type t' =
 *     | Blah of Blah.t
 * end = B
 * ```
 * Basically when a module B implements another module A, we need
 * to make B a varient type of A and also make sure that any other
 * properties in A are then copied over in B to simulate proper
 * inheritence.
 * 
 *)

module Token = Lexer.Token
module Lex_env = Lexer.Lex_env

(* Supporting types *)
module rec Arguments : sig
  type t = 
    | SpreadElement of SpreadElement.t list
    | Expression of Expression.t list
end = Arguments

and Identifier : sig
  type t = string
end = Identifier

and IdentifierName : sig
  type t = string
end = IdentifierName

and Label : sig
  type t = string
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
    isAsync: bool;
    isGenerator: bool;
    params: FormalParameters.t;
    body: FunctionBody.t;
  }
end = Function

(* Node classes *)
and Node : sig
  (* Currently not implementing `TypeIndicator` *)
  type t' = 
    | Program of Program.t
    | Statement of Statement.t
    | Expression of Expression.t
    | PropertyName of PropertyName.t 
    | ObjectProperty of ObjectProperty.t
    | ImportDeclaration of ImportDeclaration.t
    | ExportDeclaration of ExportDeclaration.t
    | VariableReference  of VariableReference.t
    | BindingWithDefault of BindingWithDefault.t
    | MemberAssignmentTarget of MemberAssignmentTarget.t
    | ArrayBinding of ArrayBinding.t
    | ObjectBinding of ObjectBinding.t
    | BindingProperty of BindingProperty.t
    | AssignmentTargetWithDefault of AssignmentTargetWithDefault.t
    | ArrayAssignmentTarget of ArrayAssignmentTarget.t
    | ObjectAssignmentTarget of ObjectAssignmentTarget.t
    | AssignmentTargetProperty of AssignmentTargetProperty.t
    | ClassElement of ClassElement.t
    | ImportSpecifier of ImportSpecifier.t
    | ExportFromSpecifier of ExportFromSpecifier.t
    | ExportLocalSpecifier of ExportLocalSpecifier.t
    | Block of Block.t
    | CatchClause of CatchClause.t
    | Directive of Directive.t
    | FormalParameters of FormalParameters.t
    | FunctionBody of FunctionBody.t
    | SpreadElement of SpreadElement.t
    | Super of Super.t
    | SwitchCase of SwitchCase.t
    | SwitchDefault of SwitchDefault.t
    | TemplateElement of TemplateElement.t
    | VariableDeclaration of VariableDeclaration.t
    | VariableDeclarator of VariableDeclarator.t
end = Node

and Program : sig
  type t =
    | Script of Script.t
    | Module of Module.t
end = Program

and Statement : sig
  type t = 
    | IterationStatement of IterationStatement.t
    | ClassDeclaration of ClassDeclaration.t
    | BlockStatement of BlockStatement.t
    | BreakStatement of BreakStatement.t
    | ContinueStatement of ContinueStatement.t
    | DebuggerStatement of DebuggerStatement.t
    | ExpressionStatement of ExpressionStatement.t
    | IfStatement of IfStatement.t
    | LabeledStatement of LabeledStatement.t
    | ReturnStatement of ReturnStatement.t
    | SwitchStatement of SwitchStatement.t
    | SwitchStatementWithDefault of SwitchStatementWithDefault.t
    | ThrowStatement of ThrowStatement.t
    | TryCatchStatement of TryCatchStatement.t
    | TryFinallyStatement of TryFinallyStatement.t
    | VariableDeclarationStatement of VariableDeclarationStatement.t
    | WithStatement of WithStatement.t
    | FunctionDeclaration of FunctionDeclaration.t
    | EmptyStatement
end = Statement

and IterationStatement : sig
  type t = {
    body:  Statement.t;
  }
end = IterationStatement

and Expression : sig
  type t = 
    | MemberExpression of MemberExpression.t
    | ClassExpression of ClassExpression.t
    | LiteralBooleanExpression of LiteralBooleanExpression.t
    | LiteralInfinityExpression of LiteralInfinityExpression.t
    | LiteralNullExpression of LiteralNullExpression.t
    | LiteralNumericExpression of LiteralNumericExpression.t
    | LiteralRegExpExpression of LiteralRegExpExpression.t
    | LiteralStringExpression of LiteralStringExpression.t
    | ArrayExpression of ArrayExpression.t
    | ArrowExpression of ArrowExpression.t
    | AssignmentExpression of AssignmentExpression.t
    | BinaryExpression of BinaryExpression.t
    | CallExpression of CallExpression.t
    | CompoundAssignmentExpression of CompoundAssignmentExpression.t
    | ConditionalExpression of ConditionalExpression.t
    | FunctionExpression of FunctionExpression.t
    | IdentifierExpression of IdentifierExpression.t
    | NewExpression of NewExpression.t
    | NewTargetExpression of NewTargetExpression.t
    | ObjectExpression of ObjectExpression.t
    | UnaryExpression of UnaryExpression.t
    | TemplateExpression of TemplateExpression.t
    | ThisExpression of ThisExpression.t
    | UpdateExpression of UpdateExpression.t
    | YieldExpression of YieldExpression.t
    | YieldGeneratorExpression of YieldGeneratorExpression.t
    | AwaitExpression of AwaitExpression.t
end = Expression

and MemberExpression : sig
  type t = { _object: t' }
  and t' =
    | Expression of Expression.t
    | Super of Super.t
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
