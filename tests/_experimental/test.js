/** 
 * K-ary Tree
 * {root} Node, the root of the tree
 * 
 * Asymptotic time complexities
 * +---------------------------+
 * | isLeaf           | O(1)   |
 * | emptySubtree     | O(n)   |
 * | emptyTree        | O(n)   |
 * | search           | O(n)   |
 * +---------------------------+
 * 
 * TODO: let user set a custom `equal` function
 * 
 */

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

/** @description
 * Empties the entire tree.
 * @param {void}
 * @return {void}
 */
KaryTree.prototype.emptyTree = function(){
  if(this.root === null){
    throw new Error("Attempted to empty a nulled tree in KaryTree.emptyTree");
  }
  this.emptySubtree(this.root);
  this.root = null;
}

/** @description
 * Search for a node in the tree.
 * @param {*} the data of the node being searched for
 * @param {Node} [node] the node where to begin the insertion. This should be left blank when called
 *                      because the function default to inserting at the root.
 * @return {Node || false} returns the node if found, and false if not found
 */
KaryTree.prototype.search = function(data, node){
  if(node === null){
    return false;
  }

  // If node isn't defined, default to the root
  if(typeof node === 'undefined'){
    node = this.root;
  }

  // Check if current node is the one we're looking for
  if(JSON.stringify(data) ===  JSON.stringify(node.data)){
    return node;
  }

  // Traverse through each child
  node.children.map(function(child){
    search(data, child);
  });
}

module.exports = KaryTree;