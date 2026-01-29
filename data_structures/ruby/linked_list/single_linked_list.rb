class Node
  attr_accessor :data, :next_node

  def initialize(data = nil)
    @data = data
    @next_node = nil
  end
end

class LinkedList
  def initialize
    @head = nil
    @size = 0
  end

  def add_first(data)
    # create new node instance, storing reference to element data
    new_node = Node.new(data)

    # set new node's next_node to reference the old head node
    new_node.next_node = @head

    # set head to reference the new node
    @head = new_node

    # increment the node count
    @size += 1
  end

  def add_last(data)
    # create new node instance, storing reference to element data
    new_node = Node.new(data)

    # if the list is empty, set the new node as the head
    if @head.nil?
      @head = new_node
    else
      # traverse to the last node
      current_node = @head
      current_node = current_node.next_node while current_node.next_node

      # set the last node's next_node to the new node
      current_node.next_node = new_node
    end

    # increment the node count
    @size += 1
  end

  def insert_at(index, data)
    # check if given index is out of range
    raise IndexError, "Index out of range!" if index < 0 || index > @size

    # if given index is zero, we alias to add_first
    if index == 0
      add_first(data)
      return
    end

    # if given index is equal to the size, we alias to add_last
    if index == @size
      add_last(data)
      return
    end

    current_node = @head

    # traverse to the node before the insertion
    (index - 1).times do
      current_node = current_node.next_node
    end

    # create new node instance, storing reference to element data
    new_node = Node.new(data)

    # since we are at the node before the insertion,
    # set new node's next_node to reference the next node
    new_node.next_node = current_node.next_node

    # set the current node's next_node to the new node
    current_node.next_node = new_node

    # increment the node count
    @size += 1
  end

  def delete_first
    # if the list is empty, raise an exception
    raise "List is empty!" if @head.nil?

    # set the head to the next node
    @head = @head.next_node

    # decrement the node count
    @size -= 1
  end

  def delete_last
    # if the list is empty, raise an exception
    raise "List is empty!" if @head.nil?

    # if there's only one node, set the head to nil
    if @head.next_node.nil?
      @head = nil
    else
      # traverse to the second-to-last node
      current_node = @head
      current_node = current_node.next_node while current_node.next_node.next_node

      # set the second-to-last node's next_node to nil
      current_node.next_node = nil
    end

    # decrement the node count
    @size -= 1
  end

  def delete(data)
    # deletes the first occurrence of a node with the specified data
    # if the list is empty, raise an exception
    raise "List is empty!" if @head.nil?

    # if the head node contains the data to be deleted
    if @head.data == data
      @head = @head.next_node
      @size -= 1
      return
    end

    # traverse the list to find the node before the node to be deleted
    current_node = @head
    while current_node.next_node
      if current_node.next_node.data == data
        # remove the node by updating the next_node reference
        current_node.next_node = current_node.next_node.next_node
        @size -= 1
        return
      end
      current_node = current_node.next_node
    end

    # if the data is not found, raise an exception
    raise "Data not found in the list!"
  end

  def length
    @size
  end

  def to_s
    return "[]" if @head.nil?

    current_node = @head
    elements = []
    while current_node
      elements << current_node.data.to_s
      current_node = current_node.next_node
    end

    "[" + elements.join(" -> ") + "]"
  end

  # For testing purposes
  attr_reader :head
end

if __FILE__ == $PROGRAM_NAME
  # create a new linked list
  linked_list = LinkedList.new

  # add elements to the linked list
  linked_list.add_first(1)
  linked_list.add_last(2)
  linked_list.add_last(3)
  linked_list.add_first(0)

  # print the linked list
  puts linked_list

  # assert that elements are in the correct order before deletion
  raise "Test failed" unless linked_list.head.data == 0
  raise "Test failed" unless linked_list.head.next_node.data == 1
  raise "Test failed" unless linked_list.head.next_node.next_node.data == 2
  raise "Test failed" unless linked_list.head.next_node.next_node.next_node.data == 3
  raise "Test failed" unless linked_list.head.next_node.next_node.next_node.next_node.nil?

  # delete an element from the linked list
  linked_list.delete(2)

  # print the linked list
  puts linked_list

  # assert that elements are in the correct order
  raise "Test failed" unless linked_list.head.data == 0
  raise "Test failed" unless linked_list.head.next_node.data == 1
  raise "Test failed" unless linked_list.head.next_node.next_node.data == 3
  raise "Test failed" unless linked_list.head.next_node.next_node.next_node.nil?

  puts "All tests passed!"
end
