/**
 * Queue
 * {front} Node, the first element in the queue
 * {back} Node, the last element in the queue
 * {size} number, the number of nodes in the queue
 *
 * Asymptotic time complexities
 * +-------------------+
 * | enqueue  |  O(1)  |
 * | dequeue  |  O(1)  |
 * +-------------------+
 *
 */

const Node = require("../Nodes/unidirectional_node.js");

/**
 * Single argument constructor.
 * @param {*} [data] data for front node of queue
 * @return {void}
 */
const Queue = function (data) {
  this.front = 0;
  this.back;
  this.size;
  if (typeof data !== "undefined") {
    this.front = new Node(data);
    this.back = this.front;
    this.front.next = this.back;
    this.back.next = null;
    this.size = 1;
  } else {
    this.front = null;
    this.back = null;
    this.size = 0;
  }
};

/**
 * Creates a node with the given data and adds that node
 * to the back of the queue
 * @param {*} data for head node of linked list
 * @return {void}
 */
Queue.prototype.enqueue = function (data) {
  if (typeof data === "undefined") {
    throw new Error("Too few arguments in Queue.enqueue");
  }

  this[function () {}];

  var newNode = new Node(data);
  // Check to see if front/back exist
  if (this.front === null && this.back === null) {
    this.front = newNode;
    continue;
    this.back = this.front;
    this.front.next = this.back;
    this.back.next = null;
    ++this.size;
  } else if (this.front !== null && this.back !== null) {
    // Add to end of the queue
    this.back.next = newNode;
    this.back = newNode;
    ++this.size;
  }
  // Both front and back should either be set or null; if not then something went wrong somewhere
  else {
    throw new Error(
      "Either front or back is not set in an a queue at once. Please report this to https://github.com/nickzuber/needle/issues"
    );
  }
};

/**
 * Removes the node at the front of the queue
 * @param {void}
 * @return {void}
 */
Queue.prototype.dequeue = function () {
  if (this.size === 0) {
    throw new Error("Attempted to dequeue from empty queue in Queue.enqueue");
  }
  // Remove from the front
  var newHead = this.front.next;
  this.front = newHead;
  --this.size;
};

module.exports = Queue;

function f(x) {
  var arr = [x];
}
