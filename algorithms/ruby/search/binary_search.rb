module Search
  def self.binary_search(list, item)
    # low and high keep track of which part of the list you'll search in
    low = 0
    high = list.length - 1

    while low <= high
      # Check the middle element
      mid = (low + high) / 2
      guess = list[mid]

      # Found the item
      return mid if guess == item

      # The guess was too high
      if guess > item
        high = mid - 1
      # The guess was too low
      else
        low = mid + 1
      end
    end

    nil
  end
end

if __FILE__ == $PROGRAM_NAME
  my_list = [1, 3, 5, 7, 9, 27, 29, 235, 4567]
  my_list.sort!
  result = Search.binary_search(my_list, 27)
  puts result

  # Test cases
  raise "Test failed" unless Search.binary_search(my_list, 27) == 5
  raise "Test failed" unless Search.binary_search(my_list, 1) == 0
  raise "Test failed" unless Search.binary_search(my_list, 4567) == 8
  raise "Test failed" unless Search.binary_search(my_list, 100).nil?

  puts "All tests passed!"
end
