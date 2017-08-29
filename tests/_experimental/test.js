
'use strict'

// VariableDeclarations
let foo = 1

// VariableDeclarations
let foo, bar = 1

// BinaryExpressions and VariableDeclarations
const foo, bar, baz = 1 - i + (3 * 5) / 5

// CallExpressions and SpreadElements
const item = foo((2 + 3), baz, 1 + 2, ...bar)

// IdentifierExpression
foo

// AssignmentExpression
foo = 1 + 2, 3, 4

// AssignmentExpression
foo = bar

// AssignmentExpression
foo = false

// AssignmentExpression with BinaryExpression
foo = 1 - i + (3 * 4) / 5 

// BinaryExpression with AssignmentExpression
// a = 1 + b   ,   c    ,    d = e
a = 1 + b, c, d = e

// BinaryExpression with AssignmentExpression
foo, bar = 2 + true

// BinaryExpression with AssignmentExpression
foo = bar = baz
