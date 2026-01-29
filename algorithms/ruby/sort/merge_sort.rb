module Sort
  def self.merge_sort(arr)
    # Base case: array of length 0 or 1 is already sorted
    return arr if arr.length <= 1

    # Divide the array into two halves
    mid = arr.length / 2
    left_half = arr[0...mid]
    right_half = arr[mid..]

    # Recursively sort both halves
    left_half = merge_sort(left_half)
    right_half = merge_sort(right_half)

    # Merge the sorted halves
    merge(left_half, right_half)
  end

  def self.merge(left, right)
    result = []
    i = 0
    j = 0

    # Compare elements from both arrays and add the smaller one to the result
    while i < left.length && j < right.length
      if left[i] <= right[j]
        result << left[i]
        i += 1
      else
        result << right[j]
        j += 1
      end
    end

    # Add any remaining elements from both arrays
    result.concat(left[i..])
    result.concat(right[j..])

    result
  end
end

if __FILE__ == $PROGRAM_NAME
  arr = [38, 27, 43, 3, 9, 82, 10]
  sorted_arr = Sort.merge_sort(arr)
  puts sorted_arr.inspect

  # Test cases
  raise "Test failed" unless Sort.merge_sort([38, 27, 43, 3, 9, 82, 10]) == [3, 9, 10, 27, 38, 43, 82]
  raise "Test failed" unless Sort.merge_sort([1]) == [1]
  raise "Test failed" unless Sort.merge_sort([]) == []
  raise "Test failed" unless Sort.merge_sort([5, 3, 6, 2, 10]) == [2, 3, 5, 6, 10]

  puts "All tests passed!"
end
