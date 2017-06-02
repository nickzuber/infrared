'use strict';

const Node = require('../Nodes/multidirectional_tree_node.js');

/**
 * Creates an empty k-ary tree.
 * @param {Node} the root node
 * @return {void}
 */
const KaryTree = function(data){
  this.root = new Node(data);
}

/** @description
 * Checks if the given node is a leaf.
 * @param {Node} the node being checked
 * @return {boolean} returns true if node is a leaf
 */
KaryTree.prototype.isLeaf = function(node){
  if(typeof node === 'undefined' || node === null){
    throw new TypeError('Attempting to check the leaves of undefined node in KaryTree.isLeaf');
  }
  return (node.children.length === 0);
}

/** @description
 * Empties the subtree of the given node.
 * @param {Node} the root of the subtree being emptied
 * @return {void}
 */
KaryTree.prototype.emptySubtree = function(node){
  if(node === null){
    return;
  }
  node.children.map(function(child){
    this.emptySubtree(child);
    child = null;
  }.bind(this));
  node = null;
}