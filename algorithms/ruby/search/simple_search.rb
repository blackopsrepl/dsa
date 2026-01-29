module Search
  def self.simple_search(list, item)
    list.each_with_index do |element, i|
      return i if element == item
    end
    nil
  end
end

if __FILE__ == $PROGRAM_NAME
  my_list = [1, 3, 5, 7, 9, 27, 6, 235, 55]
  result = Search.simple_search(my_list, 6)
  puts result

  # Test cases
  raise "Test failed" unless Search.simple_search(my_list, 6) == 6
  raise "Test failed" unless Search.simple_search(my_list, 1) == 0
  raise "Test failed" unless Search.simple_search(my_list, 55) == 8
  raise "Test failed" unless Search.simple_search(my_list, 100).nil?

  puts "All tests passed!"
end
