'use strict'

const ast = require('./public/test.json')

function createTreeNode (astNode, rule) {
    if (astNode === null) return false

    const node = astNode[Object.keys(astNode)[0]]
    const name = node.type
    let description = addDescription(node)
    const children = []

    for (let attr in node) {
        let child = node[attr]
        if (Array.isArray(child)) {
            for (let i = 0; i < child.length; i++) {
                let treeNode = createTreeNode(child[i], attr)
                if (treeNode) children.push(treeNode)
            }
        } else if (typeof child === 'object') {
            let treeNode = createTreeNode(child, attr)
            if (treeNode) children.push(treeNode)
        }
    }
    return { name, children, description, rule }
}

function addDescription (node) {
    switch (node.type) {
        case 'BindingIdentifier':
            return node.name
        case 'LiteralNumericExpression':
            return node.value
        case 'LiteralNumericExpression':
            return node.value
        case 'IdentifierExpression':
            return node.name
        case 'BinaryExpression':
            return node.operator
        case 'VariableDeclaration':
            return node.kind
        case 'Directive':
            return node.rawValue
        case 'AssignmentTargetIdentifier':
            return node.name
        default:
            return null
    }
}

const treeNode = createTreeNode(ast)

// Logging here prints to stdout which we pipe into a file later
// Kinda jank but does the job
console.log(JSON.stringify(treeNode, null, 4))