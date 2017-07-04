
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

(* i imported these but i don't think we need them here?
module Token = Lexer.Token
module Lex_env = Lexer.Lex_env
*)


(*** Supporting types ***)

(* typedef *)
module rec Arguments : sig
  type t = 
    | SpreadElement of SpreadElement.t list
    | Expression of Expression.t list
end = Arguments

(* typedef *)
and Identifier : sig
  type t = string
end = Identifier

(* typedef *)
and IdentifierName : sig
  type t = string
end = IdentifierName

(* typedef *)
and Label : sig
  type t = string
end = Label

(* typedef *)
and VariableDeclarationKind : sig
  type t = 
    | Var
    | Let
    | Const 
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
end = UnaryOperator

(* typedef *)
and UpdateOperator : sig
  type t = 
    | Increment           (*    ++    *)
    | Decrement           (*    --    *)
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
end = IterationStatement

and Expression : sig
  module Attributes : sig
    type t = {
      _type: string;
    }
  end
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
end = VariableReference


(*** Bindings ***)

(* typedef *)
and BindingPattern : sig
  type t = 
    | ObjectBinding of ObjectBinding.t
    | ArrayBinding of ArrayBinding.t
end = BindingPattern

(* typedef *)
and Binding : sig
  type t =
    | BindingPattern of BindingPattern.t
    | BindingIdentifier of BindingIdentifier.t
end = Binding

(* typedef *)
and SimpleAssignmentTarget : sig
  type t = 
    | AssignmentTargetIdentifier of AssignmentTargetIdentifier.t
    | MemberAssignmentTarget of MemberAssignmentTarget.t
end = SimpleAssignmentTarget

(* typedef *)
and AssignmentTargetPattern : sig
  type t = 
    | ObjectAssignmentTarget of ObjectAssignmentTarget.t
    | ArrayAssignmentTarget of ArrayAssignmentTarget.t
end = AssignmentTargetPattern

(* typedef *)
and AssignmentTarget : sig
  type t = 
    | AssignmentTargetPattern of AssignmentTargetPattern.t
    | SimpleAssignmentTarget of SimpleAssignmentTarget.t
end = AssignmentTarget 

(* typedef *)
and Parameter : sig
  type t = 
    | Binding of Binding.t
    | BindingWithDefault of BindingWithDefault.t
end = Parameter

and BindingWithDefault : sig
  type t = {
    _type: string;
    binding: Binding.t;
    init: Expression.t;
  }
end = BindingWithDefault

and BindingIdentifier : sig
  type t = {
    _type: string;
    name: Identifier.t;
  }
end = BindingIdentifier

and AssignmentTargetIdentifier : sig
  type t = {
    _type: string;
    name: Identifier.t;
  }
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
end = MemberAssignmentTarget

and ComputedMemberAssignmentTarget : sig
  type t = {
    _type: string;
    _object: _object_types;
    expression: Expression.t;
  }
  and _object_types = 
    | Expression of Expression.t
    | Super of Super.t
end = ComputedMemberAssignmentTarget

and StaticMemberAssignmentTarget : sig
  type _object_types = 
    | Expression of Expression.t
    | Super of Super.t
  type t = {
    _type: string;
    _object: _object_types;
    property: IdentifierName.t;
  }
end = StaticMemberAssignmentTarget

and ArrayBinding : sig
  type element = 
    | Binding of Binding.t
    | BindingWithDefault of BindingWithDefault.t
  type t = {
    _type: string;
    elements: element list option;
    rest: Binding.t option;
  }
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
end = BindingProperty

and BindingPropertyIdentifier : sig
  type t = {
    _type: string;
    binding: BindingIdentifier.t;
    init: Expression.t option;
  }
end = BindingPropertyIdentifier

and BindingPropertyProperty : sig
  type binding = 
    | Binding of Binding.t
    | BindingWithDefault of BindingWithDefault.t
  type t = {
    _type: string;
    name: PropertyName.t;
    binding: binding;
  }
end = BindingPropertyProperty

and AssignmentTargetWithDefault : sig
  type t = {
    _type: string;
    binding: AssignmentTarget.t;
    init: Expression.t;
  }
end = AssignmentTargetWithDefault

and ArrayAssignmentTarget : sig
  (* ex. [a, b] = [10, 20] *)
  type element = 
    | AssignmentTarget of AssignmentTarget.t
    | AssignmentTargetWithDefault of AssignmentTargetWithDefault.t
  type t = {
    _type: string;
    elements: element list option;
    rest: AssignmentTarget.t option;
  }
end = ArrayAssignmentTarget

and ObjectAssignmentTarget : sig
  type t = {
    _type: string;
    properties: AssignmentTargetProperty.t list;
  }
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
end = AssignmentTargetProperty

and AssignmentTargetPropertyIdentifier : sig
  type t = {
    _type: string;
    binding: AssignmentTargetIdentifier.t;
    init: Expression.t option;
  }
end = AssignmentTargetPropertyIdentifier

and AssignmentTargetPropertyProperty : sig
  type binding = 
    | AssignmentTarget of AssignmentTarget.t
    | AssignmentTargetWithDefault of AssignmentTargetWithDefault.t
  type t = {
    _type: string;
    name: PropertyName.t;
    binding: binding;
  }
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
    (* True iff `IsStatic` of ClassElement is true *)
    isStatic: bool;
    method_: MethodDefinition.t;
    super: Expression.t option;
    elements: ClassElement.t list;
  }
end = ClassElement


(*** Modules ***)

and Module : sig
  type item = 
    | ImportDeclaration of ImportDeclaration.t
    | ExportDeclaration of ExportDeclaration.t
    | Statement of Statement.t
  type t = {
    _type: string;
    directives: Directive.t list;
    items: item list;
  }
end = Module

and Import : sig
  type t = {
    _type: string;
    moduleSpecifier: string;
    (* `ImportedDefaultBinding`, if present *)
    defaultBinding: BindingIdentifier.t option;
    namedImports: ImportSpecifier.t list;
  }
end = Import

and ImportNamespace : sig
  type t = {
    _type: string;
    moduleSpecifier: string;
    (* `ImportedDefaultBinding`, if present *)
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

(* <- workin thru shit right over here -> *)

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
  type declaration = 
    | FunctionDeclaration of FunctionDeclaration.t
    | ClassDeclaration of ClassDeclaration.t
    | VariableDeclaration of VariableDeclaration.t
  type t = {
    _type: string;
    declaration: declaration;
  }
end = Export

and ExportDefault : sig
  type body = 
    | FunctionDeclaration of FunctionDeclaration.t
    | ClassDeclaration of ClassDeclaration.t
    | Expression of Expression.t
  type t = {
    _type: string;
    body: body;
  }
end = ExportDefault

and ExportFromSpecifier : sig
  type t = {
    _type: string;
    name: IdentifierName.t;
    exportedName: IdentifierName.t option;
  }
end = ExportFromSpecifier

and ExportLocalSpecifier : sig
  type t = {
    _type: string;
    name: IdentifierExpression.t;
    exportedName: IdentifierName.t option;
  }
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
end = Method

and Getter : sig
  type t = {
    _type: string;
    body: FunctionBody.t;
    name: PropertyName.t;
  }
end = Getter

and Setter : sig
  type t = {
    _type: string;
    body: FunctionBody.t;
    name: PropertyName.t;
    (* The `PropertySetParameterList` *)
    param: Parameter.t;
  }
end = Setter

and DataProperty : sig
  type t = {
    _type: string;
    name: PropertyName.t;
    expression: Expression.t;
  }
end = DataProperty

and ShorthandProperty : sig
  type t = {
    _type: string;
    name: IdentifierExpression.t;
  }
end = ShorthandProperty

and ComputedPropertyName : sig
  type t = {
    _type: string;
    expression: Expression.t;
  }
end = ComputedPropertyName

and StaticPropertyName : sig
  type t = {
    _type: string;
    value: string;
  }
end = StaticPropertyName


(*** Literals ***)

and LiteralBooleanExpression : sig
  type t = {
    _type: string;
    value: bool;
  }
end = LiteralBooleanExpression

and LiteralInfinityExpression : sig
  type t = {
    _type: string;
  }
end = LiteralInfinityExpression

and LiteralNullExpression : sig
  type t = {
    _type: string;
  }
end = LiteralNullExpression

and LiteralNumericExpression : sig
  type t = {
    _type: string;
    (* value: double *)
    value: float;
  }
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
end = LiteralRegExpExpression

and LiteralStringExpression : sig
  type t = {
    _type: string;
    value: string;
  }
end = LiteralStringExpression


(*** Other Expressions ***)

and ArrayExpression : sig
  type element = 
    | SpreadElement of SpreadElement.t
    | Expression of Expression.t
  type t = {
    _type: string;
    elements: element list option;
  }
end = ArrayExpression

and ArrowExpression : sig
  type body = 
    | FunctionBody of FunctionBody.t
    | Expression of Expression.t
  type t = {
    _type: string;
    (* True for `AsyncArrowFunction`, false otherwise *)
    isAsync: bool;
    params: FormalParameters.t;
    body: body;
  }
end = ArrowExpression

and AssignmentExpression : sig
  type t = {
    _type: string;
    (* The `LeftHandSideExpression` *)
    binding: AssignmentTarget.t;
    (* The `AssignmentExpression` following the `=` *)
    expression: Expression.t;
  }
end = AssignmentExpression

and BinaryExpression : sig
  type t = {
    _type: string;
    operator: BinaryOperator.t;
    left: Expression.t;
    right: Expression.t;
  }
end = BinaryExpression

and CallExpression : sig
  type callee = 
    | Expression of Expression.t
    | Super of Super.t
  type t = {
    _type: string;
    callee: callee;
    arguments: Arguments.t;
  }
end = CallExpression

and CompoundAssignmentExpression : sig
  type t = {
    _type: string;
    (* The `LeftHandSideExpression` *)
    binding: SimpleAssignmentTarget.t;
    (* The `AssignmentExpression` *)
    expression: Expression.t;
  }
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
end = FunctionExpression

and IdentifierExpression : sig
  type t = {
    _type: string;
    name: Identifier.t;
  }
end = IdentifierExpression

and NewExpression : sig
  type t = {
    _type: string;
    callee: Expression.t;
    arguments: Arguments.t;
  }
end = NewExpression

and NewTargetExpression : sig
  (* ex. https://stackoverflow.com/a/32458960/5055063 *)
  type t = {
    _type: string;
  }
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
  type _object_types = 
    | Expression of Expression.t
    | Super of Super.t
  type t = {
    _type: string;
    _object: _object_types;
    (* The name of the property to be accessed *)
    property: IdentifierName.t;
  }
end = StaticMemberExpression

and TemplateExpression : sig
  type element = 
    | Expression of Expression.t
    | TemplateElement of TemplateElement.t
  type t = {
    _type: string;
    (* The second `MemberExpression` or `CallExpression`, if present *)
    tag: Expression.t option;
    (* The contents of the template. This list must be alternating 
       TemplateElements and Expressions, beginning and ending with TemplateElement *)
    elements: element list;
  }
end = TemplateExpression

and ThisExpression : sig
  type t = {
    _type: string;
  }
end = ThisExpression

and UpdateExpression : sig
  type t = {
    _type: string;
    (* True for `UpdateExpression :: ++ LeftHandSideExpression` and `UpdateExpression :: -- LeftHandSideExpression`, false otherwise *)
    isPrefix: bool;
    operator: UpdateOperator.t;
    operand: SimpleAssignmentTarget.t;
  }
end = UpdateExpression

and YieldExpression : sig
  type t = {
    _type: string;
    expression: Expression.t option;
  }
end = YieldExpression

and YieldGeneratorExpression : sig
  type t = {
    _type: string;
    expression: Expression.t;
  }
end = YieldGeneratorExpression

and AwaitExpression : sig
  type t = {
    _type: string;
    expression: Expression.t;
  }
end = AwaitExpression


(*** Other Statements ***)

and BlockStatement : sig
  type t = {
    _type: string;
    block: Block.t;
  }
end = BlockStatement

and BreakStatement : sig
  type t = {
    _type: string;
    label: Label.t option;
  }
end = BreakStatement

and ContinueStatement : sig
  type t = {
    _type: string;
    label: Label.t option;
  }
end = ContinueStatement
 
and DebuggerStatement : sig
  type t = {
    _type: string;
  }
end = DebuggerStatement

and DoWhileStatement : sig
  type t = {
    _type: string;
    body: Statement.t;
    test: Expression.t;
  }
end = DoWhileStatement

and EmptyStatement : sig
  type t = {
    _type: string;
  }
end = EmptyStatement

and ExpressionStatement : sig
  type t = {
    _type: string;
    expression: Expression.t;
  }
end = ExpressionStatement

and ForInStatement : sig
  type declaration = 
    | VariableDeclaration of VariableDeclaration.t
    | AssignmentTarget of AssignmentTarget.t
  type t = {
    _type: string;
    body: Statement.t;
    (* The expression or declaration before `in` *)
    left: declaration;
    (* The expression after `in` *)
    right: Expression.t;
  }
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
end = ForOfStatement

and ForStatement : sig
  type declaration = 
    | VariableDeclaration of VariableDeclaration.t
    | AssignmentTarget of AssignmentTarget.t
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
end = IfStatement

and LabeledStatement : sig
  type t = {
    _type: string;
    label: Label.t;
    body: Statement.t;
  }
end = LabeledStatement

and ReturnStatement : sig
  type t = {
    _type: string;
    expression: Expression.t;
  }
end = ReturnStatement

and SwitchStatement : sig
  type t = {
    _type: string;
    discriminant: Expression.t;
    cases: SwitchCase.t list;
  }
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
end = SwitchStatementWithDefault

and ThrowStatement : sig
  type t = {
    _type: string;
    expression: Expression.t;
  }
end = ThrowStatement

and TryCatchStatement : sig
  type t = {
    _type: string;
    body: Block.t;
    catchClause: CatchClause.t;
  }
end = TryCatchStatement

and TryFinallyStatement : sig
  type t = {
    _type: string;
    body: Block.t;
    catchClause: CatchClause.t option;
    finalizer: Block.t;
  }
end = TryFinallyStatement

and VariableDeclarationStatement : sig
  type t = {
    _type: string;
    declaration: VariableDeclaration.t;
  }
end = VariableDeclarationStatement

and WhileStatement : sig
  type t = {
    _type: string;
    body: Statement.t;
    test: Expression.t;
  }
end = WhileStatement

and WithStatement : sig
  type t = {
    _type: string;
    _object: Expression.t;
    body: Statement.t;
  }
end = WithStatement

(*** Other Nodes ***)

and Block : sig
  type t = {
    _type: string;
    statements: Statement.t list;
  }
end = Block

and CatchClause : sig
  type t = {
    _type: string;
    binding: Binding.t;
    body: Block.t;
  }
end = CatchClause

and Directive : sig
  type t = {
    _type: string;
    rawValue: string;
  }
end = Directive

and FormalParameters : sig
  type t = {
    _type: string;
    items: Parameter.t list;
    rest: Binding.t option;
  }
end = FormalParameters

and FunctionBody : sig
  type t = {
    _type: string;
    directives: Directive.t list;
    statements: Statement.t list;
  }
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
end = FunctionDeclaration

and Script : sig
  type t = {
    _type: string;
    directives: Directive.t list;
    statements: Statement.t list;
  }
end = Script

and SpreadElement : sig
  type t = {
    _type: string;
    expression: Expression.t;
  }
end = SpreadElement

and Super : sig
  type t = {
    _type: string;
  }
end = Super

and SwitchCase : sig
  type t = {
    _type: string;
    test: Expression.t;
    consequent: Statement.t list;
  }
end = SwitchCase

and SwitchDefault : sig
  type t = {
    _type: string;
    consequent: Statement.t list;
  }
end = SwitchDefault

and TemplateElement : sig
  type t = {
    _type: string;
    rawValue: string;
  }
end = TemplateElement

and VariableDeclaration : sig
  type t = {
    _type: string;
    kind: VariableDeclarationKind.t;
    declarators: VariableDeclarator.t list;
  }
end = VariableDeclaration

and VariableDeclarator : sig
  type t = {
    _type: string;
    binding: Binding.t;
    init: Expression.t option;
  }
end = VariableDeclarator

type t = {
  loc: Loc.t;
  node: t';
}

and t' =
  | IdentifierName of IdentifierName.t
