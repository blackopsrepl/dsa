module Sort
  def self.topological_sort_graph(graph, nodes)
    # Compute in-degree for each node
    in_degree = nodes.to_h { |node| [node, 0] }
    graph.each do |_node, neighbors|
      neighbors.each do |neighbor|
        in_degree[neighbor] += 1
      end
    end

    # Initialize queue with nodes having in-degree 0
    queue = nodes.select { |node| in_degree[node] == 0 }
    result = []

    until queue.empty?
      current = queue.shift
      result << current

      # Reduce in-degree for neighbors
      (graph[current] || []).each do |neighbor|
        in_degree[neighbor] -= 1
        queue << neighbor if in_degree[neighbor] == 0
      end
    end

    if result.length != nodes.length
      raise "Graph contains a cycle or disconnected components."
    end

    result
  end
end

def test_topological_sort
  # Test case 1: Simple linear graph
  graph1 = {
    'A' => ['B'],
    'B' => ['C'],
    'C' => []
  }
  nodes1 = ['A', 'B', 'C']

  puts "Test case 1:"
  puts "Graph: #{graph1}"
  puts "Nodes: #{nodes1}"
  result1 = Sort.topological_sort_graph(graph1, nodes1)
  puts "Result: #{result1}\n\n"
  raise "Test failed" unless result1 == ['A', 'B', 'C']

  # Test case 2: Diamond shaped graph
  graph2 = {
    'A' => ['B', 'C'],
    'B' => ['D'],
    'C' => ['D'],
    'D' => []
  }
  nodes2 = ['A', 'B', 'C', 'D']

  puts "Test case 2:"
  puts "Graph: #{graph2}"
  puts "Nodes: #{nodes2}"
  result2 = Sort.topological_sort_graph(graph2, nodes2)
  puts "Result: #{result2}\n\n"

  # Check A is first and D is last
  raise "Test failed" unless result2[0] == 'A'
  raise "Test failed" unless result2[-1] == 'D'

  # Check B and C are in middle (order can vary)
  raise "Test failed" unless result2[1..2].sort == ['B', 'C']

  # Test case 3: Empty graph
  graph3 = {}
  nodes3 = []

  puts "Test case 3:"
  puts "Graph: #{graph3}"
  puts "Nodes: #{nodes3}"
  result3 = Sort.topological_sort_graph(graph3, nodes3)
  puts "Result: #{result3}\n\n"
  raise "Test failed" unless result3 == []

  # Test case 4: Graph with cycle should raise error
  graph4 = {
    'A' => ['B'],
    'B' => ['C'],
    'C' => ['A']
  }
  nodes4 = ['A', 'B', 'C']

  puts "Test case 4:"
  puts "Graph: #{graph4}"
  puts "Nodes: #{nodes4}"
  begin
    Sort.topological_sort_graph(graph4, nodes4)
    raise "Expected error for cyclic graph"
  rescue RuntimeError => e
    puts "Caught expected error: #{e.message}\n\n"
  end

  puts "All test cases passed!"
end

if __FILE__ == $PROGRAM_NAME
  test_topological_sort
end
