'use strict'

const ast = require('./public/test.json')

function createTreeNode (astNode) {
    if (astNode === null) return false
    const node = astNode[Object.keys(astNode)[0]]
    const name = node.type
    const children = []
    for (let attr in node) {
        let child = node[attr]
        if (Array.isArray(child)) {
            for (let i = 0; i < child.length; i++) {
                let treeNode = createTreeNode(child[i])
                if (treeNode) children.push(treeNode)
            }
        } else if (typeof child === 'object') {
            let treeNode = createTreeNode(child)
            if (treeNode) children.push(treeNode)
        }
    }
    return { name, children }
}

const treeNode = createTreeNode(ast)

console.log(JSON.stringify(treeNode, null, 4))