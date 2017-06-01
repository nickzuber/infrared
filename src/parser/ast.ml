
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
 *   type t =
 *     | B of B.t
 * end = A
 * 
 * and B : sig 
 *   type t = { ...attributes }
 * end = B
 * ```
 * Basically when a module B implements another module A, we need
 * to make B a varient type of A and also make sure that any other
 * properties in A are then moved over to B to simulate this type of
 * inheritence.
 * 
 *)

module Token = Lexer.Token
module Lex_env = Lexer.Lex_env

(*** Supporting types ***)
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

(*! others implement this *)
and Function : sig
  type t = {
    (* True for `AsyncFunctionExpression` and `AsyncFunctionDeclaration`,
       false otherwise. *)
    isAsync: bool;
    (* True for `GeneratorExpression` and `GeneratorDeclaration`,
       false otherwise. *)
    isGenerator: bool;
    params: FormalParameters.t;
    body: FunctionBody.t;
  }
end = Function

(*** Node classes ***)
and Node : sig
  type t = 
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
  type t = 
    | DoWhileStatement of DoWhileStatement.t
    | ForInStatement of ForInStatement.t
    | ForOfStatement of ForOfStatement.t
    | ForStatement of ForStatement.t
    | WhileStatement of WhileStatement.t
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
  type t = 
    | ComputedMemberExpression of ComputedMemberExpression.t
    | StaticMemberExpression of StaticMemberExpression.t
end = MemberExpression

and PropertyName : sig
  type t =
    | ComputedPropertyName of ComputedPropertyName.t
    | StaticPropertyName of StaticPropertyName.t
end = PropertyName

and ObjectProperty : sig
  type t =
    | NamedObjectProperty of NamedObjectProperty.t
    | ShorthandProperty of ShorthandProperty.t
end = ObjectProperty

and NamedObjectProperty : sig
  and t = 
    | MethodDefinition of MethodDefinition.t
    | DataProperty of DataProperty.t
end = NamedObjectProperty

and MethodDefinition : sig
  type t = 
    | Method of Method.t
    | Getter of Getter.t
    | Setter of Setter.t
end = MethodDefinition

and ImportDeclaration : sig
  type t = 
    | Import of Import.t
    | ImportNamespace of ImportNamespace.t
end = ImportDeclaration

and ExportDeclaration : sig
  type t =
    | ExportAllFrom of ExportAllFrom.t
    | ExportFrom of ExportFrom.t
    | ExportLocals of ExportLocals.t
    | Export of Export.t
    | ExportDefault of ExportDefault.t
end = ExportDeclaration

and VariableReference : sig
  type t = 
    | BindingIdentifier of BindingIdentifier.t
    | AssignmentTargetIdentifier of AssignmentTargetIdentifier.t
end = VariableReference

(*** Bindings ***)
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

and MemberAssignmentTarget : sig
  type t = 
    | ComputedMemberAssignmentTarget of ComputedMemberAssignmentTarget.t
    | StaticMemberAssignmentTarget of StaticMemberAssignmentTarget.t
end = MemberAssignmentTarget

and ComputedMemberAssignmentTarget : sig
  type t = {
    _type: string;
    expression: Expression.t;
  }
end = ComputedMemberAssignmentTarget

and StaticMemberAssignmentTarget : sig
  type t = {
    _type: string;
    property: IdentifierName.t;
}
end = StaticMemberAssignmentTarget

and ArrayBinding : sig
  type t = {
    _type: string;
    elements: t' list;
    rest: Binding.t option;
  }
  and t' = 
    | Binding of Binding.t
    | BindingWithDefault of BindingWithDefault.t
end = ArrayBinding

and ObjectBinding : sig
  type t = {
    _type: string;
    properties: BindingProperty.t;
  }
end = ObjectBinding

and BindingProperty : sig
  type t = 
    | BindingPropertyIdentifier of BindingPropertyIdentifier.t
    | BindingPropertyProperty of BindingPropertyProperty.t
end = BindingProperty

and BindingPropertyIdentifier : sig
  type t = {
    _type: string;
    binding: BindingIdentifier.t;
    init: Expression.t option;
  }
end = BindingPropertyIdentifier

and BindingPropertyProperty : sig
  type t = {
    _type: string;
    name: PropertyName.t;
    binding: t';
  }
  and t' = 
    | Binding of Binding.t
    | BindingWithDefault of BindingWithDefault.t
end = BindingPropertyProperty

and AssignmentTargetWithDefault : sig
  type t = {
    _type: string;
    binding: AssignmentTarget.t;
    init: Expression.t;
  }
end = AssignmentTargetWithDefault

and ArrayAssignmentTarget : sig
  type t = {
    _type: string;
    elements: t' list;
    rest: AssignmentTarget.t option;
  }
  and t' = 
    | AssignmentTarget of AssignmentTarget.t
    | AssignmentTargetWithDefault of AssignmentTargetWithDefault.t
end = ArrayAssignmentTarget

and ObjectAssignmentTarget : sig
  type t = {
    _type: string;
    properties: AssignmentTargetProperty.t list;
  }
end = ObjectAssignmentTarget

and AssignmentTargetProperty : sig
  type t = 
    | AssignmentTargetPropertyIdentifier of AssignmentTargetPropertyIdentifier.t
    | AssignmentTargetPropertyProperty of AssignmentTargetPropertyProperty.t
end = AssignmentTargetProperty

and AssignmentTargetPropertyIdentifier : sig
  type t = {
    _type: string;
    binding: AssignmentTargetIdentifier.t;
    init: Expression.t option;
  }
end = AssignmentTargetPropertyIdentifier

and AssignmentTargetPropertyProperty : sig
  type t = {
    _type: string;
    name: PropertyName.t;
    binding: t';
  }
  and t' = 
    | AssignmentTarget of AssignmentTarget.t
    | AssignmentTargetWithDefault of AssignmentTargetWithDefault.t
end = AssignmentTargetPropertyProperty

(*** Classes ***)

(*! others implement this *)
and Class : sig
  type t = {
    super: Expression.t option;
    elements: ClassElement.t list;
  }
end = Class

and ClassExpression : sig
  type t = {
    _type: string;
    name: BindingIdentifier.t option;
    super: Expression.t option;
    elements: ClassElement.t list;
  }
end = ClassExpression

and ClassDeclaration : sig
  type t = {
    _type: string;
    name: BindingIdentifier.t;
    super: Expression.t option;
    elements: ClassElement.t list;
  }
end = ClassDeclaration

and ClassElement : sig
  type t = {
    _type: string;
    (* True iff `IsStatic` of ClassElement is true. *)
    isStatic: bool;
    method: MethodDefinition.t;
    super: Expression.t option;
    elements: ClassElement.t list;
  }
end = ClassElement

(*** Modules ***)

and Module : sig
  type t = {
    _type: string;
    directives: Directive.t list;
    items: t' list;
  }
  and t' = 
    | ImportDeclaration of ImportDeclaration.t
    | ExportDeclaration of ExportDeclaration.t
    | Statement of Statement.t
end = Module

and Import : sig
  type t = {
    _type: string;
    moduleSpecifier: string;
    (* `ImportedDefaultBinding`, if present. *)
    defaultBinding: BindingIdentifier.t option;
    namedImports: ImportSpecifier.t list;
  }
end = Import

and ImportNamespace : sig
  type t = {
    _type: string;
    moduleSpecifier: string;
    (* `ImportedDefaultBinding`, if present. *)
    defaultBinding: BindingIdentifier.t option;
    namespaceBinding: BindingIdentifier.t;
  }
end = ImportNamespace

and ImportSpecifier : sig
  type t = {
    _type: string;
    name: IdentifierName.t option;
    binding: BindingIdentifier.t;
  }
end = ImportSpecifier

(*? `export * FromClause;` *)
and ExportAllFrom : sig
  type t = {
    _type: string;
    namedExports: ExportFromSpecifier.t list;
    moduleSpecifier: string;
  }
end = ExportAllFrom

and ExportFrom : sig
  type t = {
    _type: string;
    namedExports: ExportLocalSpecifier.t list;
  }
end = ExportFrom

and ExportLocals : sig
  type t = {
    _type: string;
    namedExports: ExportLocalSpecifier.t list;
  }
end = ExportLocals

and Export : sig
  type t = {
    _type: string;
    declaration: t';
  }
  and t' = 
    | FunctionDeclaration of FunctionDeclaration.t
    | ClassDeclaration of ClassDeclaration.t
    | VariableDeclaration of VariableDeclaration.t
end = Export

and ExportDefault : sig
  type t = {
    _type: string;
    body: t';
  }
  and t' = 
    | FunctionDeclaration of FunctionDeclaration.t
    | ClassDeclaration of ClassDeclaration.t
    | Expression of Expression.t
end = ExportDefault




(* ... *)

and NewExpression : sig
  type t = {
    _type: string;
    callee: Expression.t;
    arguments: Arguments.t;
  }
end = NewExpression

and NewTargetExpression : sig
  (* https://stackoverflow.com/a/32458960/5055063 *)
  type t = { _type: string; }
end = NewTargetExpression

and ObjectExpression : sig
  type t = {
    _type: string;
    properties: ObjectProperty.t list;
  }
end = ObjectExpression

and UnaryExpression : sig
  type t = {
    _type: string;
    operator: UnaryOperator.t;
    operand: Expression.t;
  }
end = UnaryExpression

and StaticMemberExpression : sig
  type t = {
    _type: string;
    property: IdentifierName.t;
  }
end = StaticMemberExpression

and TemplateExpression : sig
  type t = {
    _type: string;
    (* The second `MemberExpression` or `CallExpression`, if present. *)
    tag: Expression.t option;
    (* The contents of the template. This list must be alternating 
       TemplateElements and Expressions, beginning and ending with TemplateElement. *)
    elements: t' list;
  }
  and t' = 
    | Expression of Expression.t
    | TemplateElement of TemplateElement.t
end = TemplateExpression

and ThisExpression : sig
  type t = { _type: string; }
end = ThisExpression

and UpdateExpression : sig
  type t = {
    _type: string;
    (* True for `UpdateExpression :: ++ LeftHandSideExpression` and
       `UpdateExpression :: -- LeftHandSideExpression`, false otherwise. *)
    isPrefix: bool;
    operator: UpdateOperator.t;
    operand: SimpleAssignmentTarget.t;
  }
end = UpdateExpression




 
