"""
Linked List Implementation using Object-Oriented Programming

This implementation uses OOP for several key advantages:
1. Performance: Direct reference manipulation provides faster operations
2. Memory efficiency: Nodes are allocated only when needed
3. Intuitive design: Nodes as objects with references naturally map to the linked list concept
4. Scalability: Avoids recursion limits and excessive copying overhead

The OOP approach allows for more efficient list operations like insertion, deletion, 
and traversal compared to functional alternatives in Python, which often hit 
performance constraints with large datasets.
"""

class _Node:
    def __init__(self, data=None):
        self._data = data
        self._next = None


class LinkedList:
    def __init__(self):
        self._head = None
        self._size = 0

    def add_first(self, data) -> None:
        # create new node instance, storing reference to element `data`
        new_node: _Node = _Node(data)
        
        # set new node's `_next` to reference the old head node
        new_node._next = self._head
        
        # set `_head` to reference the new node
        self._head = new_node
        
        # increment the node count
        self._size += 1

    def add_last(self, data) -> None:
        # create new node instance, storing reference to element `data`
        new_node: _Node = _Node(data)

        # if the list is empty, set the new node as the head
        if self._head is None:
            self._head = new_node
        else:
            # traverse to the last node
            current_node: _Node = self._head
            while current_node._next:
                current_node = current_node._next

            # set the last node's `_next` to the new node
            current_node._next = new_node

            # increment the node count
            self._size += 1

    def insert_at(self, index: int, data) -> None:
        # check if given index is out of range
        if index < 0 or index > self._size:
            raise IndexError("Index out of range!")
        
        # if given index is zero, we alias to add_first
        if index == 0:
            self.add_first(data)
            return

        # if given index is equal to the size, we alias to add_last
        if index == self._size:
            self.add_last(data)
            return
        
        current_node = self._head

        # traverse to the node before the insertion
        for _ in range(index - 1):
            current_node = current_node._next
        
        # create new node instance, storing reference to element `data`
        new_node = _Node(data)

        # since we are at the node before the insertion,
        # set new node's `_next` to reference the next node
        new_node._next = current_node._next

        # set the last node's `_next` to the new node
        current_node._next = new_node

        # increment the node count
        self._size += 1

    def delete_first(self) -> None:
        # if the list is empty, raise an exception
        if self._head is None:
            raise Exception("List is empty!")

        # set the head to the next node
        self._head = self._head._next

        # decrement the node count
        self._size -= 1

    def delete_last(self) -> None:
        # if the list is empty, raise an exception
        if self._head is None:
            raise Exception("List is empty!")

        # if there's only one node, set the head to None
        if self._head._next is None:
            self._head = None
        else:
            # traverse to the second-to-last node
            current_node: _Node = self._head
            while current_node._next._next:
                current_node = current_node._next

            # set the second-to-last node's `_next` to None
            current_node._next = None

            # decrement the node count
            self._size -= 1

    def delete(self, data) -> None:
        # deletes the first occurence of a node with the specified data
        # if the list is empty, raise an exception
        if self._head is None:
            raise Exception("List is empty!")

        # if the head node contains the data to be deleted
        if self._head._data == data:
            self._head = self._head._next
            self._size -= 1
            return

        # traverse the list to find the node before the node to be deleted
        current_node: _Node = self._head
        while current_node._next:
            if current_node._next._data == data:
                # remove the node by updating the `_next` reference
                current_node._next = current_node._next._next
                self._size -= 1
                return
            current_node = current_node._next

        # if the data is not found, raise an exception
        raise Exception("Data not found in the list!")

    def __len__(self) -> int:
        # return the size of the linked list
        return self._size
    
    def __str__(self) -> str:
        # return a string representation of the linked list
        if self._head is None:
            return "[]"
        
        current_node: _Node = self._head
        elements = []
        while current_node:
            elements.append(str(current_node._data))
            current_node = current_node._next

        return "[" + " -> ".join(elements) + "]"

if __name__ == "__main__":
    # create a new linked list
    linked_list = LinkedList()

    # add elements to the linked list
    linked_list.add_first(1)
    linked_list.add_last(2)
    linked_list.add_last(3)
    linked_list.add_first(0)

    # print the linked list
    print(linked_list)

    # assert that elements are in the correct order before deletion
    assert linked_list._head._data == 0
    assert linked_list._head._next._data == 1
    assert linked_list._head._next._next._data == 2
    assert linked_list._head._next._next._next._data == 3
    assert linked_list._head._next._next._next._next is None

    # delete an element from the linked list
    linked_list.delete(2)

    # print the linked list
    print(linked_list)

    # assert that elements are in the correct order
    assert linked_list._head._data == 0
    assert linked_list._head._next._data == 1
    assert linked_list._head._next._next._data == 3
    assert linked_list._head._next._next._next is None