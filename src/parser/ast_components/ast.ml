
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
 *   __attributes__ = { ...attributes }
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
 * A module will only have an __attributes__ field if it is not a terminal
 * module. If it is a terminal module, then technically its __attribute__
 * field would just be its type.
 *
 *)


(*** Supporting types ***)

(* typedef *)
module rec Arguments : sig
  type arguments =
    | SpreadElement of SpreadElement.t
    | Expression of Expression.t
  type t = arguments list
    [@@deriving show]
end = Arguments

(* typedef *)
and Identifier : sig
  type t = string
  [@@deriving show]
end = Identifier

(* typedef *)
and IdentifierName : sig
  type t = string
  [@@deriving show]
end = IdentifierName

(* typedef *)
and Label : sig
  type t = string
  [@@deriving show]
end = Label

(* typedef *)
and VariableDeclarationKind : sig
  type t =
    | Var
    | Let
    | Const
    [@@deriving show]
end = VariableDeclarationKind

(* typedef *)
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
    [@@deriving show]
end = CompoundAssignmentOperator

(* typedef *)
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
    | Instanceof
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
    [@@deriving show]
end = BinaryOperator

(* typedef *)
and UnaryOperator : sig
  type t =
    | Plus                (*    +     *)
    | Minus               (*    -     *)
    | Bang                (*    !     *)
    | Not                 (*    ~     *)
    | Typeof
    | Void
    | Delete
    [@@deriving show]
end = UnaryOperator

(* typedef *)
and UpdateOperator : sig
  type t =
    | Increment           (*    ++    *)
    | Decrement           (*    --    *)
    [@@deriving show]
end = UpdateOperator

(*! others implement this *)
and Function : sig
  type t = {
    (* True for `AsyncFunctionExpression` and `AsyncFunctionDeclaration`, false otherwise *)
    isAsync: bool;
    (* True for `GeneratorExpression` and `GeneratorDeclaration`, false otherwise *)
    isGenerator: bool;
    params: FormalParameters.t;
    body: FunctionBody.t;
  }
  [@@deriving show]
end = Function


(*** Node classes ***)

and Node : sig
  module Attributes : sig
    type t = {
      _type: string;
    }
  end
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
    [@@deriving show]
end = Node

and Program : sig
  module Attributes : sig
    type t = {
      _type: string;
    }
  end
  type t =
    | Script of Script.t
    | Module of Module.t
    [@@deriving show]
end = Program

and Statement : sig
  module Attributes : sig
    type t = {
      _type: string;
    }
  end
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
    [@@deriving show]
end = Statement

and IterationStatement : sig
  module Attributes : sig
    type t = {
      _type: string;
      body: Statement.t;
    }
  end
  type t =
    | DoWhileStatement of DoWhileStatement.t
    | ForInStatement of ForInStatement.t
    | ForOfStatement of ForOfStatement.t
    | ForStatement of ForStatement.t
    | WhileStatement of WhileStatement.t
    [@@deriving show]
end = IterationStatement

and Expression : sig
  module Attributes : sig
    type t = {
      _type: string;
    }
  end
  type t =
    | IdentifierExpression of IdentifierExpression.t
    | LiteralNumericExpression of LiteralNumericExpression.t
    | LiteralBooleanExpression of LiteralBooleanExpression.t
    | BinaryExpression of BinaryExpression.t
    | CallExpression of CallExpression.t
    | AssignmentExpression of AssignmentExpression.t
    (* Unimplemented in parser (below) *)
    | MemberExpression of MemberExpression.t
    | ClassExpression of ClassExpression.t
    | LiteralInfinityExpression of LiteralInfinityExpression.t
    | LiteralNullExpression of LiteralNullExpression.t
    | LiteralRegExpExpression of LiteralRegExpExpression.t
    | LiteralStringExpression of LiteralStringExpression.t
    | ArrayExpression of ArrayExpression.t
    | ArrowExpression of ArrowExpression.t
    | CompoundAssignmentExpression of CompoundAssignmentExpression.t
    | ConditionalExpression of ConditionalExpression.t
    | FunctionExpression of FunctionExpression.t
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
    [@@deriving show]
end = Expression

and MemberExpression : sig
  module Attributes : sig
    type _object_types =
      | Expression of Expression.t
      | Super of Super.t
    type t = {
      _type: string;
      _object: _object_types;
    }
  end
  type t =
    | ComputedMemberExpression of ComputedMemberExpression.t
    | StaticMemberExpression of StaticMemberExpression.t
    [@@deriving show]
end = MemberExpression

and PropertyName : sig
  module Attributes : sig
    type t = {
      _type: string;
    }
  end
  type t =
    | ComputedPropertyName of ComputedPropertyName.t
    | StaticPropertyName of StaticPropertyName.t
    [@@deriving show]
end = PropertyName

and ObjectProperty : sig
  module Attributes : sig
    type t = {
      _type: string;
    }
  end
  type t =
    | NamedObjectProperty of NamedObjectProperty.t
    | ShorthandProperty of ShorthandProperty.t
    [@@deriving show]
end = ObjectProperty

and NamedObjectProperty : sig
  module Attributes : sig
    type t = {
      _type: string;
      name: PropertyName.t;
    }
  end
  type t =
    | MethodDefinition of MethodDefinition.t
    | DataProperty of DataProperty.t
    [@@deriving show]
end = NamedObjectProperty

and MethodDefinition : sig
  module Attributes : sig
    type t = {
      _type: string;
      name: PropertyName.t;
      body: FunctionBody.t;
    }
  end
  type t =
    | Method of Method.t
    | Getter of Getter.t
    | Setter of Setter.t
    [@@deriving show]
end = MethodDefinition

and ImportDeclaration : sig
  module Attributes : sig
    type t = {
      _type: string;
      moduleSpecifier: string;
    }
  end
  type t =
    | Import of Import.t
    | ImportNamespace of ImportNamespace.t
    [@@deriving show]
end = ImportDeclaration

and ExportDeclaration : sig
  module Attributes : sig
    type t = {
      _type: string;
    }
  end
  type t =
    | ExportAllFrom of ExportAllFrom.t
    | ExportFrom of ExportFrom.t
    | ExportLocals of ExportLocals.t
    | Export of Export.t
    | ExportDefault of ExportDefault.t
    [@@deriving show]
end = ExportDeclaration

and VariableReference : sig
  module Attributes : sig
    type t = {
      _type: string;
      name: Identifier.t;
    }
  end
  type t =
    | BindingIdentifier of BindingIdentifier.t
    | AssignmentTargetIdentifier of AssignmentTargetIdentifier.t
    [@@deriving show]
end = VariableReference


(*** Bindings ***)

(* typedef *)
and BindingPattern : sig
  type t =
    | ObjectBinding of ObjectBinding.t
    | ArrayBinding of ArrayBinding.t
    [@@deriving show]
end = BindingPattern

(* typedef *)
and Binding : sig
  type t =
    | BindingPattern of BindingPattern.t
    | BindingIdentifier of BindingIdentifier.t
    [@@deriving show]
end = Binding

(* typedef *)
and SimpleAssignmentTarget : sig
  type t =
    | AssignmentTargetIdentifier of AssignmentTargetIdentifier.t
    | MemberAssignmentTarget of MemberAssignmentTarget.t
    [@@deriving show]
end = SimpleAssignmentTarget

(* typedef *)
and AssignmentTargetPattern : sig
  type t =
    | ObjectAssignmentTarget of ObjectAssignmentTarget.t
    | ArrayAssignmentTarget of ArrayAssignmentTarget.t
    [@@deriving show]
end = AssignmentTargetPattern

(* typedef *)
and AssignmentTarget : sig
  type t =
    | AssignmentTargetPattern of AssignmentTargetPattern.t
    | SimpleAssignmentTarget of SimpleAssignmentTarget.t
    [@@deriving show]
end = AssignmentTarget

(* typedef *)
and Parameter : sig
  type t =
    | Binding of Binding.t
    | BindingWithDefault of BindingWithDefault.t
    [@@deriving show]
end = Parameter

and BindingWithDefault : sig
  type t = {
    _type: string;
    binding: Binding.t;
    init: Expression.t;
  }
  [@@deriving show]
end = BindingWithDefault

and BindingIdentifier : sig
  type t = {
    _type: string;
    name: Identifier.t;
  }
  [@@deriving show]
end = BindingIdentifier

and AssignmentTargetIdentifier : sig
  type t = {
    _type: string;
    name: Identifier.t;
  }
  [@@deriving show]
end = AssignmentTargetIdentifier

and MemberAssignmentTarget : sig
  module Attributes : sig
    type _object_types =
      | Expression of Expression.t
      | Super of Super.t
    type t = {
      _type: string;
      _object: _object_types;
    }
  end
  type t =
    | ComputedMemberAssignmentTarget of ComputedMemberAssignmentTarget.t
    | StaticMemberAssignmentTarget of StaticMemberAssignmentTarget.t
    [@@deriving show]
end = MemberAssignmentTarget

and ComputedMemberAssignmentTarget : sig
  type objects =
    | Expression of Expression.t
    | Super of Super.t
    [@@deriving show]
  type t = {
    _type: string;
    _object: objects;
    expression: Expression.t;
  }
  [@@deriving show]
end = ComputedMemberAssignmentTarget

and StaticMemberAssignmentTarget : sig
  type objects =
    | Expression of Expression.t
    | Super of Super.t
    [@@deriving show]
  type t = {
    _type: string;
    _object: objects;
    property: IdentifierName.t;
  }
  [@@deriving show]
end = StaticMemberAssignmentTarget

and ArrayBinding : sig
  type element =
    | Binding of Binding.t
    | BindingWithDefault of BindingWithDefault.t
    [@@deriving show]
  type t = {
    _type: string;
    elements: element list option;
    rest: Binding.t option;
  }
  [@@deriving show]
end = ArrayBinding

and ObjectBinding : sig
  module Attributes : sig
    type t = {
      _type: string;
      properties: BindingProperty.t list;
    }
  end
  type t = {
    _type: string;
    properties: BindingProperty.t;
  }
  [@@deriving show]
end = ObjectBinding

and BindingProperty : sig
  module Attributes : sig
    type t = {
      _type: string;
    }
  end
  type t =
    | BindingPropertyIdentifier of BindingPropertyIdentifier.t
    | BindingPropertyProperty of BindingPropertyProperty.t
    [@@deriving show]
end = BindingProperty

and BindingPropertyIdentifier : sig
  type t = {
    _type: string;
    binding: BindingIdentifier.t;
    init: Expression.t option;
  }
  [@@deriving show]
end = BindingPropertyIdentifier

and BindingPropertyProperty : sig
  type binding =
    | Binding of Binding.t
    | BindingWithDefault of BindingWithDefault.t
    [@@deriving show]
  type t = {
    _type: string;
    name: PropertyName.t;
    binding: binding;
  }
  [@@deriving show]
end = BindingPropertyProperty

and AssignmentTargetWithDefault : sig
  type t = {
    _type: string;
    binding: AssignmentTarget.t;
    init: Expression.t;
  }
  [@@deriving show]
end = AssignmentTargetWithDefault

and ArrayAssignmentTarget : sig
  (* ex. [a, b] = [10, 20] *)
  type element =
    | AssignmentTarget of AssignmentTarget.t
    | AssignmentTargetWithDefault of AssignmentTargetWithDefault.t
    [@@deriving show]
  type t = {
    _type: string;
    elements: element list option;
    rest: AssignmentTarget.t option;
  }
  [@@deriving show]
end = ArrayAssignmentTarget

and ObjectAssignmentTarget : sig
  type t = {
    _type: string;
    properties: AssignmentTargetProperty.t list;
  }
  [@@deriving show]
end = ObjectAssignmentTarget

and AssignmentTargetProperty : sig
  module Attributes : sig
    type t = {
      _type: string;
    }
  end
  type t =
    | AssignmentTargetPropertyIdentifier of AssignmentTargetPropertyIdentifier.t
    | AssignmentTargetPropertyProperty of AssignmentTargetPropertyProperty.t
    [@@deriving show]
end = AssignmentTargetProperty

and AssignmentTargetPropertyIdentifier : sig
  type t = {
    _type: string;
    binding: AssignmentTargetIdentifier.t;
    init: Expression.t option;
  }
  [@@deriving show]
end = AssignmentTargetPropertyIdentifier

and AssignmentTargetPropertyProperty : sig
  type binding =
    | AssignmentTarget of AssignmentTarget.t
    | AssignmentTargetWithDefault of AssignmentTargetWithDefault.t
    [@@deriving show]
  type t = {
    _type: string;
    name: PropertyName.t;
    binding: binding;
  }
  [@@deriving show]
end = AssignmentTargetPropertyProperty


(*** Classes ***)

(*! others implement this *)
and Class : sig
  type t = {
    super: Expression.t option;
    elements: ClassElement.t list;
  }
  [@@deriving show]
end = Class

and ClassExpression : sig
  type t = {
    _type: string;
    name: BindingIdentifier.t option;
    super: Expression.t option;
    elements: ClassElement.t list;
  }
  [@@deriving show]
end = ClassExpression

and ClassDeclaration : sig
  type t = {
    _type: string;
    name: BindingIdentifier.t;
    super: Expression.t option;
    elements: ClassElement.t list;
  }
  [@@deriving show]
end = ClassDeclaration

and ClassElement : sig
  type t = {
    _type: string;
    (* True iff `IsStatic` of ClassElement is true *)
    isStatic: bool;
    method_: MethodDefinition.t;
    super: Expression.t option;
    elements: ClassElement.t list;
  }
  [@@deriving show]
end = ClassElement


(*** Modules ***)

and Module : sig
  type item =
    | ImportDeclaration of ImportDeclaration.t
    | ExportDeclaration of ExportDeclaration.t
    | Statement of Statement.t
    [@@deriving show]
  type t = {
    _type: string;
    directives: Directive.t list;
    items: item list;
  }
  [@@deriving show]
end = Module

and Import : sig
  type t = {
    _type: string;
    moduleSpecifier: string;
    (* `ImportedDefaultBinding`, if present *)
    defaultBinding: BindingIdentifier.t option;
    namedImports: ImportSpecifier.t list;
  }
  [@@deriving show]
end = Import

and ImportNamespace : sig
  type t = {
    _type: string;
    moduleSpecifier: string;
    (* `ImportedDefaultBinding`, if present *)
    defaultBinding: BindingIdentifier.t option;
    namespaceBinding: BindingIdentifier.t;
  }
  [@@deriving show]
end = ImportNamespace

and ImportSpecifier : sig
  type t = {
    _type: string;
    name: IdentifierName.t option;
    binding: BindingIdentifier.t;
  }
  [@@deriving show]
end = ImportSpecifier

(*? `export * FromClause;` *)
and ExportAllFrom : sig
  type t = {
    _type: string;
    namedExports: ExportFromSpecifier.t list;
    moduleSpecifier: string;
  }
  [@@deriving show]
end = ExportAllFrom

and ExportFrom : sig
  type t = {
    _type: string;
    namedExports: ExportLocalSpecifier.t list;
  }
  [@@deriving show]
end = ExportFrom

and ExportLocals : sig
  type t = {
    _type: string;
    namedExports: ExportLocalSpecifier.t list;
  }
  [@@deriving show]
end = ExportLocals

and Export : sig
  type declaration =
    | FunctionDeclaration of FunctionDeclaration.t
    | ClassDeclaration of ClassDeclaration.t
    | VariableDeclaration of VariableDeclaration.t
    [@@deriving show]
  type t = {
    _type: string;
    declaration: declaration;
  }
  [@@deriving show]
end = Export

and ExportDefault : sig
  type body =
    | FunctionDeclaration of FunctionDeclaration.t
    | ClassDeclaration of ClassDeclaration.t
    | Expression of Expression.t
    [@@deriving show]
  type t = {
    _type: string;
    body: body;
  }
  [@@deriving show]
end = ExportDefault

and ExportFromSpecifier : sig
  type t = {
    _type: string;
    name: IdentifierName.t;
    exportedName: IdentifierName.t option;
  }
  [@@deriving show]
end = ExportFromSpecifier

and ExportLocalSpecifier : sig
  type t = {
    _type: string;
    name: IdentifierExpression.t;
    exportedName: IdentifierName.t option;
  }
  [@@deriving show]
end = ExportLocalSpecifier


(*** Property Definition ***)

and Method : sig
  type t = {
    _type: string;
    (* True for `AsyncMethod`, false otherwise *)
    isAsync: bool;
    (* True for `GeneratorMethod`, false otherwise *)
    isGenerator: bool;
    params: FormalParameters.t;
  }
  [@@deriving show]
end = Method

and Getter : sig
  type t = {
    _type: string;
    body: FunctionBody.t;
    name: PropertyName.t;
  }
  [@@deriving show]
end = Getter

and Setter : sig
  type t = {
    _type: string;
    body: FunctionBody.t;
    name: PropertyName.t;
    (* The `PropertySetParameterList` *)
    param: Parameter.t;
  }
  [@@deriving show]
end = Setter

and DataProperty : sig
  type t = {
    _type: string;
    name: PropertyName.t;
    expression: Expression.t;
  }
  [@@deriving show]
end = DataProperty

and ShorthandProperty : sig
  type t = {
    _type: string;
    name: IdentifierExpression.t;
  }
  [@@deriving show]
end = ShorthandProperty

and ComputedPropertyName : sig
  type t = {
    _type: string;
    expression: Expression.t;
  }
  [@@deriving show]
end = ComputedPropertyName

and StaticPropertyName : sig
  type t = {
    _type: string;
    value: string;
  }
  [@@deriving show]
end = StaticPropertyName


(*** Literals ***)

and LiteralBooleanExpression : sig
  type t = {
    _type: string;
    value: bool;
  }
  [@@deriving show]
end = LiteralBooleanExpression

and LiteralInfinityExpression : sig
  type t = {
    _type: string;
  }
  [@@deriving show]
end = LiteralInfinityExpression

and LiteralNullExpression : sig
  type t = {
    _type: string;
  }
  [@@deriving show]
end = LiteralNullExpression

and LiteralNumericExpression : sig
  type t = {
    _type: string;
    (* value: double *)
    value: float;
  }
  [@@deriving show]
end = LiteralNumericExpression

and LiteralRegExpExpression : sig
  type t = {
    _type: string;
    pattern: string;
    (* Whether the `g` flag is present *)
    global: bool;
    (* Whether the `i` flag is present *)
    ignoreCase: bool;
    (* Whether the `m` flag is present *)
    multiLine: bool;
    (* Whether the `y` flag is present *)
    sticky: bool;
    (* Whether the `u` flag is present *)
    unicode: bool;
  }
  [@@deriving show]
end = LiteralRegExpExpression

and LiteralStringExpression : sig
  type t = {
    _type: string;
    value: string;
  }
  [@@deriving show]
end = LiteralStringExpression


(*** Other Expressions ***)

and ArrayExpression : sig
  type element =
    | SpreadElement of SpreadElement.t
    | Expression of Expression.t
  [@@deriving show]
  type t = {
    _type: string;
    elements: element list option;
  }
  [@@deriving show]
end = ArrayExpression

and ArrowExpression : sig
  type body =
    | FunctionBody of FunctionBody.t
    | Expression of Expression.t
  [@@deriving show]
  type t = {
    _type: string;
    (* True for `AsyncArrowFunction`, false otherwise *)
    isAsync: bool;
    params: FormalParameters.t;
    body: body;
  }
  [@@deriving show]
end = ArrowExpression

and AssignmentExpression : sig
  type t = {
    _type: string;
    (* The `LeftHandSideExpression` *)
    binding: AssignmentTarget.t;
    (* The `AssignmentExpression` following the `=` *)
    expression: Expression.t;
  }
  [@@deriving show]
end = AssignmentExpression

and BinaryExpression : sig
  type t = {
    _type: string;
    left: Expression.t;
    operator: BinaryOperator.t;
    right: Expression.t;
  }
  [@@deriving show]
end = BinaryExpression

and CallExpression : sig
  type callee =
    | Expression of Expression.t
    | Super of Super.t
  [@@deriving show]
  type t = {
    _type: string;
    callee: callee;
    arguments: Arguments.t;
  }
  [@@deriving show]
end = CallExpression

and CompoundAssignmentExpression : sig
  type t = {
    _type: string;
    (* The `LeftHandSideExpression` *)
    binding: SimpleAssignmentTarget.t;
    (* The `AssignmentExpression` *)
    expression: Expression.t;
  }
  [@@deriving show]
end = CompoundAssignmentExpression

and ComputedMemberExpression : sig
  type _object_types =
    | Expression of Expression.t
    | Super of Super.t
  type t = {
    _type: string;
    _object: _object_types;
    (* The expression resolving to the name of the property to be accessed *)
    expression: Expression.t;
  }
  [@@deriving show]
end = ComputedMemberExpression

and ConditionalExpression : sig
  type t = {
    _type: string;
    (* The `LogicalORExpression` *)
    test: Expression.t;
    (* The first `AssignmentExpression` *)
    consequent: Expression.t;
    (* The second `AssignmentExpression` *)
    alternate: Expression.t;
  }
  [@@deriving show]
end = ConditionalExpression

and FunctionExpression : sig
  type t = {
    _type: string;
    name: BindingIdentifier.t option;
    (* True for `AsyncFunctionExpression` and `AsyncFunctionDeclaration`, false otherwise *)
    isAsync: bool;
    (* True for `GeneratorExpression` and `GeneratorDeclaration`, false otherwise *)
    isGenerator: bool;
    params: FormalParameters.t;
    body: FunctionBody.t;
  }
  [@@deriving show]
end = FunctionExpression

and IdentifierExpression : sig
  type t = {
    _type: string;
    name: Identifier.t;
  }
  [@@deriving show]
end = IdentifierExpression

and NewExpression : sig
  type t = {
    _type: string;
    callee: Expression.t;
    arguments: Arguments.t;
  }
  [@@deriving show]
end = NewExpression

and NewTargetExpression : sig
  (* ex. https://stackoverflow.com/a/32458960/5055063 *)
  type t = {
    _type: string;
  }
  [@@deriving show]
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
  [@@deriving show]
end = UnaryExpression

and StaticMemberExpression : sig
  type _object_types =
    | Expression of Expression.t
    | Super of Super.t
    [@@deriving show]
  type t = {
    _type: string;
    _object: _object_types;
    (* The name of the property to be accessed *)
    property: IdentifierName.t;
  }
  [@@deriving show]
end = StaticMemberExpression

and TemplateExpression : sig
  type element =
    | Expression of Expression.t
    | TemplateElement of TemplateElement.t
    [@@deriving show]
  type t = {
    _type: string;
    (* The second `MemberExpression` or `CallExpression`, if present *)
    tag: Expression.t option;
    (* The contents of the template. This list must be alternating
       TemplateElements and Expressions, beginning and ending with TemplateElement *)
    elements: element list;
  }
  [@@deriving show]
end = TemplateExpression

and ThisExpression : sig
  type t = {
    _type: string;
  }
  [@@deriving show]
end = ThisExpression

and UpdateExpression : sig
  type t = {
    _type: string;
    (* True for `UpdateExpression :: ++ LeftHandSideExpression` and `UpdateExpression :: -- LeftHandSideExpression`, false otherwise *)
    isPrefix: bool;
    operator: UpdateOperator.t;
    operand: SimpleAssignmentTarget.t;
  }
  [@@deriving show]
end = UpdateExpression

and YieldExpression : sig
  type t = {
    _type: string;
    expression: Expression.t option;
  }
  [@@deriving show]
end = YieldExpression

and YieldGeneratorExpression : sig
  type t = {
    _type: string;
    expression: Expression.t;
  }
  [@@deriving show]
end = YieldGeneratorExpression

and AwaitExpression : sig
  type t = {
    _type: string;
    expression: Expression.t;
  }
  [@@deriving show]
end = AwaitExpression


(*** Other Statements ***)

and BlockStatement : sig
  type t = {
    _type: string;
    block: Block.t;
  }
  [@@deriving show]
end = BlockStatement

and BreakStatement : sig
  type t = {
    _type: string;
    label: Label.t option;
  }
  [@@deriving show]
end = BreakStatement

and ContinueStatement : sig
  type t = {
    _type: string;
    label: Label.t option;
  }
  [@@deriving show]
end = ContinueStatement

and DebuggerStatement : sig
  type t = {
    _type: string;
  }
  [@@deriving show]
end = DebuggerStatement

and DoWhileStatement : sig
  type t = {
    _type: string;
    body: Statement.t;
    test: Expression.t;
  }
  [@@deriving show]
end = DoWhileStatement

and EmptyStatement : sig
  type t = {
    _type: string;
  }
  [@@deriving show]
end = EmptyStatement

and ExpressionStatement : sig
  type t = {
    _type: string;
    expression: Expression.t;
  }
  [@@deriving show]
end = ExpressionStatement

and ForInStatement : sig
  type declaration =
    | VariableDeclaration of VariableDeclaration.t
    | AssignmentTarget of AssignmentTarget.t
    [@@deriving show]
  type t = {
    _type: string;
    body: Statement.t;
    (* The expression or declaration before `in` *)
    left: declaration;
    (* The expression after `in` *)
    right: Expression.t;
  }
  [@@deriving show]
end = ForInStatement

and ForOfStatement : sig
  type declaration =
    | VariableDeclaration of VariableDeclaration.t
    | AssignmentTarget of AssignmentTarget.t
  type t = {
    _type: string;
    body: Statement.t;
    (* The expression or declaration before `of` *)
    left: declaration;
    (* The expression after `of` *)
    right: Expression.t;
  }
  [@@deriving show]
end = ForOfStatement

and ForStatement : sig
  type declaration =
    | VariableDeclaration of VariableDeclaration.t
    | AssignmentTarget of AssignmentTarget.t
    [@@deriving show]
  type t = {
    _type: string;
    body: Statement.t;
    (* The expression or declaration before the first `;`, if present *)
    init: declaration option;
    (* The expression before the second `;`, if present *)
    test: Expression.t option;
    (* The expression after the second `;`, if present *)
    udpate: Expression.t option;
  }
  [@@deriving show]
end = ForStatement

and IfStatement : sig
  type t = {
    _type: string;
    test: Expression.t;
    (* The first `Statement` *)
    consequent: Statement.t;
    (* The second `Statement`, if present *)
    alternate: Statement.t option;
  }
  [@@deriving show]
end = IfStatement

and LabeledStatement : sig
  type t = {
    _type: string;
    label: Label.t;
    body: Statement.t;
  }
  [@@deriving show]
end = LabeledStatement

and ReturnStatement : sig
  type t = {
    _type: string;
    expression: Expression.t;
  }
  [@@deriving show]
end = ReturnStatement

and SwitchStatement : sig
  type t = {
    _type: string;
    discriminant: Expression.t;
    cases: SwitchCase.t list;
  }
  [@@deriving show]
end = SwitchStatement

and SwitchStatementWithDefault : sig
  type t = {
    _type: string;
    discriminant: Expression.t;
    (* The `CaseClauses` before the `DefaultClause` *)
    preDefaultCases: SwitchCase.t list;
    (* The `DefaultClause` *)
    defaultCase: SwitchDefault.t;
    (* The `CaseClauses` after the `DefaultClause` *)
    postDefaultCases: SwitchCase.t list;
  }
  [@@deriving show]
end = SwitchStatementWithDefault

and ThrowStatement : sig
  type t = {
    _type: string;
    expression: Expression.t;
  }
  [@@deriving show]
end = ThrowStatement

and TryCatchStatement : sig
  type t = {
    _type: string;
    body: Block.t;
    catchClause: CatchClause.t;
  }
  [@@deriving show]
end = TryCatchStatement

and TryFinallyStatement : sig
  type t = {
    _type: string;
    body: Block.t;
    catchClause: CatchClause.t option;
    finalizer: Block.t;
  }
  [@@deriving show]
end = TryFinallyStatement

and VariableDeclarationStatement : sig
  type t = {
    _type: string;
    declaration: VariableDeclaration.t;
  }
  [@@deriving show]
end = VariableDeclarationStatement

and WhileStatement : sig
  type t = {
    _type: string;
    body: Statement.t;
    test: Expression.t;
  }
  [@@deriving show]
end = WhileStatement

and WithStatement : sig
  type t = {
    _type: string;
    _object: Expression.t;
    body: Statement.t;
  }
  [@@deriving show]
end = WithStatement

(*** Other Nodes ***)

and Block : sig
  type t = {
    _type: string;
    statements: Statement.t list;
  }
  [@@deriving show]
end = Block

and CatchClause : sig
  type t = {
    _type: string;
    binding: Binding.t;
    body: Block.t;
  }
  [@@deriving show]
end = CatchClause

and Directive : sig
  type t = {
    _type: string;
    rawValue: string;
  }
  [@@deriving show]
end = Directive

and FormalParameters : sig
  type t = {
    _type: string;
    items: Parameter.t list;
    rest: Binding.t option;
  }
  [@@deriving show]
end = FormalParameters

and FunctionBody : sig
  type t = {
    _type: string;
    directives: Directive.t list;
    statements: Statement.t list;
  }
  [@@deriving show]
end = FunctionBody

and FunctionDeclaration : sig
  type t = {
    _type: string;
    (* True for `AsyncFunctionExpression` and `AsyncFunctionDeclaration`, false otherwise *)
    isAsync: bool;
    (* True for `GeneratorExpression` and `GeneratorDeclaration`, false otherwise *)
    isGenerator: bool;
    params: FormalParameters.t;
    body: FunctionBody.t;
  }
  [@@deriving show]
end = FunctionDeclaration

and Script : sig
  type t = {
    _type: string;
    directives: Directive.t list;
    statements: Statement.t list;
  }
  [@@deriving show]
end = Script

and SpreadElement : sig
  type t = {
    _type: string;
    expression: Expression.t;
  }
  [@@deriving show]
end = SpreadElement

and Super : sig
  type t = {
    _type: string;
  }
  [@@deriving show]
end = Super

and SwitchCase : sig
  type t = {
    _type: string;
    test: Expression.t;
    consequent: Statement.t list;
  }
  [@@deriving show]
end = SwitchCase

and SwitchDefault : sig
  type t = {
    _type: string;
    consequent: Statement.t list;
  }
  [@@deriving show]
end = SwitchDefault

and TemplateElement : sig
  type t = {
    _type: string;
    rawValue: string;
  }
  [@@deriving show]
end = TemplateElement

and VariableDeclaration : sig
  type t = {
    _type: string;
    kind: VariableDeclarationKind.t;
    declarators: VariableDeclarator.t list;
  }
  [@@deriving show]
end = VariableDeclaration

and VariableDeclarator : sig
  type t = {
    _type: string;
    binding: BindingIdentifier.t;
    init: Expression.t option;
  }
  [@@deriving show]
end = VariableDeclarator
