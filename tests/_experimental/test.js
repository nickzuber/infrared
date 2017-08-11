
'use strict'

// BinaryExpressions and VariableDeclarations
const foo, bar, baz = 1 - i + (3 * 4) / 5
// CallExpressions and SpreadElements
const item = foo((2 + 3), baz, 2, ...bar)
// IdentifierExpression
foo
// AssignmentExpression
foo = 2
// AssignmentExpression with BinaryExpression
foo = 1 - i + (3 * 4) / 5 %
// BinaryExpression with AssignmentExpression
foo, bar = 2 + true