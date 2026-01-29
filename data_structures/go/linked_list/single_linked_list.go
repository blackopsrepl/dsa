package main

import (
	"errors"
	"fmt"
	"strings"
)

type Node struct {
	Data interface{}
	Next *Node
}

type LinkedList struct {
	head *Node
	size int
}

func NewLinkedList() *LinkedList {
	return &LinkedList{head: nil, size: 0}
}

func (ll *LinkedList) AddFirst(data interface{}) {
	// create new node instance, storing reference to element data
	newNode := &Node{Data: data}

	// set new node's Next to reference the old head node
	newNode.Next = ll.head

	// set head to reference the new node
	ll.head = newNode

	// increment the node count
	ll.size++
}

func (ll *LinkedList) AddLast(data interface{}) {
	// create new node instance, storing reference to element data
	newNode := &Node{Data: data}

	// if the list is empty, set the new node as the head
	if ll.head == nil {
		ll.head = newNode
	} else {
		// traverse to the last node
		currentNode := ll.head
		for currentNode.Next != nil {
			currentNode = currentNode.Next
		}

		// set the last node's Next to the new node
		currentNode.Next = newNode
	}

	// increment the node count
	ll.size++
}

func (ll *LinkedList) InsertAt(index int, data interface{}) error {
	// check if given index is out of range
	if index < 0 || index > ll.size {
		return errors.New("index out of range")
	}

	// if given index is zero, we alias to AddFirst
	if index == 0 {
		ll.AddFirst(data)
		return nil
	}

	// if given index is equal to the size, we alias to AddLast
	if index == ll.size {
		ll.AddLast(data)
		return nil
	}

	currentNode := ll.head

	// traverse to the node before the insertion
	for i := 0; i < index-1; i++ {
		currentNode = currentNode.Next
	}

	// create new node instance, storing reference to element data
	newNode := &Node{Data: data}

	// since we are at the node before the insertion,
	// set new node's Next to reference the next node
	newNode.Next = currentNode.Next

	// set the current node's Next to the new node
	currentNode.Next = newNode

	// increment the node count
	ll.size++
	return nil
}

func (ll *LinkedList) DeleteFirst() error {
	// if the list is empty, return an error
	if ll.head == nil {
		return errors.New("list is empty")
	}

	// set the head to the next node
	ll.head = ll.head.Next

	// decrement the node count
	ll.size--
	return nil
}

func (ll *LinkedList) DeleteLast() error {
	// if the list is empty, return an error
	if ll.head == nil {
		return errors.New("list is empty")
	}

	// if there's only one node, set the head to nil
	if ll.head.Next == nil {
		ll.head = nil
	} else {
		// traverse to the second-to-last node
		currentNode := ll.head
		for currentNode.Next.Next != nil {
			currentNode = currentNode.Next
		}

		// set the second-to-last node's Next to nil
		currentNode.Next = nil
	}

	// decrement the node count
	ll.size--
	return nil
}

func (ll *LinkedList) Delete(data interface{}) error {
	// deletes the first occurrence of a node with the specified data
	// if the list is empty, return an error
	if ll.head == nil {
		return errors.New("list is empty")
	}

	// if the head node contains the data to be deleted
	if ll.head.Data == data {
		ll.head = ll.head.Next
		ll.size--
		return nil
	}

	// traverse the list to find the node before the node to be deleted
	currentNode := ll.head
	for currentNode.Next != nil {
		if currentNode.Next.Data == data {
			// remove the node by updating the Next reference
			currentNode.Next = currentNode.Next.Next
			ll.size--
			return nil
		}
		currentNode = currentNode.Next
	}

	// if the data is not found, return an error
	return errors.New("data not found in the list")
}

func (ll *LinkedList) Length() int {
	return ll.size
}

func (ll *LinkedList) Head() *Node {
	return ll.head
}

func (ll *LinkedList) String() string {
	if ll.head == nil {
		return "[]"
	}

	var elements []string
	currentNode := ll.head
	for currentNode != nil {
		elements = append(elements, fmt.Sprintf("%v", currentNode.Data))
		currentNode = currentNode.Next
	}

	return "[" + strings.Join(elements, " -> ") + "]"
}

func main() {
	// create a new linked list
	linkedList := NewLinkedList()

	// add elements to the linked list
	linkedList.AddFirst(1)
	linkedList.AddLast(2)
	linkedList.AddLast(3)
	linkedList.AddFirst(0)

	// print the linked list
	fmt.Println(linkedList)

	// assert that elements are in the correct order before deletion
	if linkedList.Head().Data != 0 {
		panic("Test failed: head should be 0")
	}
	if linkedList.Head().Next.Data != 1 {
		panic("Test failed: second node should be 1")
	}
	if linkedList.Head().Next.Next.Data != 2 {
		panic("Test failed: third node should be 2")
	}
	if linkedList.Head().Next.Next.Next.Data != 3 {
		panic("Test failed: fourth node should be 3")
	}
	if linkedList.Head().Next.Next.Next.Next != nil {
		panic("Test failed: fifth node should be nil")
	}

	// delete an element from the linked list
	linkedList.Delete(2)

	// print the linked list
	fmt.Println(linkedList)

	// assert that elements are in the correct order
	if linkedList.Head().Data != 0 {
		panic("Test failed: head should be 0")
	}
	if linkedList.Head().Next.Data != 1 {
		panic("Test failed: second node should be 1")
	}
	if linkedList.Head().Next.Next.Data != 3 {
		panic("Test failed: third node should be 3")
	}
	if linkedList.Head().Next.Next.Next != nil {
		panic("Test failed: fourth node should be nil")
	}

	fmt.Println("All tests passed!")
}
