module Sort
  def self.quicksort(arr)
    # base case
    return arr if arr.length <= 1

    # recursive case
    pivot = arr[arr.length / 2]

    left = arr.select { |x| x < pivot }
    middle = arr.select { |x| x == pivot }
    right = arr.select { |x| x > pivot }

    quicksort(left) + middle + quicksort(right)
  end
end

if __FILE__ == $PROGRAM_NAME
  result = Sort.quicksort([3, 6, 8, 10, 1, 2, 1])
  puts result.inspect

  # Test cases
  raise "Test failed" unless Sort.quicksort([3, 6, 8, 10, 1, 2, 1]) == [1, 1, 2, 3, 6, 8, 10]
  raise "Test failed" unless Sort.quicksort([1]) == [1]
  raise "Test failed" unless Sort.quicksort([]) == []
  raise "Test failed" unless Sort.quicksort([5, 3, 6, 2, 10]) == [2, 3, 5, 6, 10]

  puts "All tests passed!"
end
