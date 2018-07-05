'use strict';

/** @private
 * Removes the element within the linked list that has the given key.
 * @param {*} the key to find
 * @param {DoublyLinkedList} the linked list to delete from
 * @return {void}
 */
function removeCustom(key, list){
  var indexToDelete = 0,
      curNode = list.head,
      hashedKey = hash(key),
      nodeFound = false;

  // Search for node to delete by comparing key hashes
  while(curNode !== null){
    if(hash(curNode.data) === hashedKey){
      nodeFound = true;
      break;
    }
    ++indexToDelete;
    curNode = curNode.next;
  }

  // If node was found, delete at that index
  if(nodeFound){
    list.removeNth(indexToDelete);
    return;
  }else{
    throw new Error("Unable to find a non null entry within the internal linked list. Please report this to https://github.com/nickzuber/needle/issues");
  }
}
