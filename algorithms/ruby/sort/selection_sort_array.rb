module Sort
  def self.find_smallest_in_array(arr)
    # stores the smallest value
    smallest = arr[0]

    # stores the index of the smallest value
    smallest_index = 0

    arr.each_with_index do |element, i|
      if element < smallest
        smallest = element
        smallest_index = i
      end
    end

    smallest_index
  end

  def self.selection_sort_array(arr)
    arr = arr.dup # Don't mutate the original array
    new_arr = []

    until arr.empty?
      # finds the smallest element in the array
      smallest = find_smallest_in_array(arr)

      # adds it to the new array
      new_arr << arr.delete_at(smallest)
    end

    new_arr
  end
end

if __FILE__ == $PROGRAM_NAME
  result = Sort.selection_sort_array([5, 3, 6, 2, 10])
  puts result.inspect

  # Test cases
  raise "Test failed" unless Sort.selection_sort_array([5, 3, 6, 2, 10]) == [2, 3, 5, 6, 10]
  raise "Test failed" unless Sort.selection_sort_array([1]) == [1]
  raise "Test failed" unless Sort.selection_sort_array([]) == []
  raise "Test failed" unless Sort.selection_sort_array([3, 1, 2]) == [1, 2, 3]

  puts "All tests passed!"
end
