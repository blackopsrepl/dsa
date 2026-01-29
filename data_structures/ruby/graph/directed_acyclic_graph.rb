require 'set'

class DAG
  def initialize
    # Adjacency list to store the graph (task -> list of dependent tasks)
    @graph = Hash.new { |h, k| h[k] = [] }

    # Store task descriptions
    @tasks = {}
  end

  def add_task(task_id, description)
    @tasks[task_id] = description
  end

  def add_dependency(from_task, to_task)
    unless @tasks.key?(from_task) && @tasks.key?(to_task)
      raise "Both tasks must exist in the DAG."
    end

    @graph[from_task] << to_task
  end

  def has_cycle?
    visited = Set.new
    rec_stack = Set.new

    dfs = lambda do |task|
      visited.add(task)
      rec_stack.add(task)

      @graph[task].each do |neighbor|
        if !visited.include?(neighbor)
          return true if dfs.call(neighbor)
        elsif rec_stack.include?(neighbor)
          return true
        end
      end

      rec_stack.delete(task)
      false
    end

    @tasks.keys.each do |task|
      next if visited.include?(task)

      return true if dfs.call(task)
    end

    false
  end

  def topological_sort
    raise "DAG contains a cycle, cannot perform topological sort." if has_cycle?

    # Compute in-degree for each task
    in_degree = @tasks.keys.to_h { |task| [task, 0] }
    @graph.each do |_task, neighbors|
      neighbors.each do |neighbor|
        in_degree[neighbor] += 1
      end
    end

    # Initialize queue with tasks having in-degree 0
    queue = @tasks.keys.select { |task| in_degree[task] == 0 }
    result = []

    until queue.empty?
      current = queue.shift
      result << current

      # Reduce in-degree for neighbors
      @graph[current].each do |neighbor|
        in_degree[neighbor] -= 1
        queue << neighbor if in_degree[neighbor] == 0
      end
    end

    if result.length != @tasks.length
      raise "DAG contains a cycle or disconnected components."
    end

    result
  end

  def print_tasks(task_order)
    puts "Task Execution Order:"
    task_order.each do |task|
      puts "#{task}: #{@tasks[task]}"
    end
  end

  attr_reader :graph, :tasks
end

def check_and_print_order(dag)
  if !dag.has_cycle?
    puts "\n\nNo cycles detected in the DAG.\n\n"
    task_order = dag.topological_sort
    dag.print_tasks(task_order)

    # Build dependency summary
    dependencies = []
    dag.graph.each do |task, neighbors|
      neighbors.each do |neighbor|
        dependencies << "#{task} (#{dag.tasks[task]}) -> #{neighbor} (#{dag.tasks[neighbor]})"
      end
    end

    # Print dependency summary
    puts "\nDependency Summary:"
    dependencies.sort.each { |dep| puts dep }
  else
    puts "\n\nCycle detected in the DAG.\n\n"
  end
end

def main
  dag = DAG.new

  # Add tasks
  dag.add_task("A", "Research topic")
  dag.add_task("B", "Create slides")
  dag.add_task("C", "Practice delivery")
  dag.add_task("D", "Finalize presentation")

  # Add dependencies
  dag.add_dependency("A", "B")  # B depends on A
  dag.add_dependency("B", "C")  # C depends on B
  dag.add_dependency("B", "D")  # D depends on B
  dag.add_dependency("C", "D")  # D depends on C

  # Check for cycles and print order
  check_and_print_order(dag)

  # Create a more complex DAG for software development lifecycle
  dag = DAG.new

  # Add tasks for different phases
  dag.add_task("REQ", "Requirements gathering")
  dag.add_task("SPEC", "Write technical specification")
  dag.add_task("DESIGN", "System architecture design")
  dag.add_task("DB", "Database schema design")
  dag.add_task("API", "API design")
  dag.add_task("UI", "UI/UX design")
  dag.add_task("CODE_BE", "Backend implementation")
  dag.add_task("CODE_FE", "Frontend implementation")
  dag.add_task("TEST_UNIT", "Unit testing")
  dag.add_task("TEST_INT", "Integration testing")
  dag.add_task("TEST_E2E", "End-to-end testing")
  dag.add_task("DOC", "Documentation")
  dag.add_task("REVIEW", "Code review")
  dag.add_task("DEPLOY", "Deployment")

  # Add dependencies
  dag.add_dependency("REQ", "SPEC")
  dag.add_dependency("SPEC", "DESIGN")
  dag.add_dependency("DESIGN", "DB")
  dag.add_dependency("DESIGN", "API")
  dag.add_dependency("DESIGN", "UI")
  dag.add_dependency("DB", "CODE_BE")
  dag.add_dependency("API", "CODE_BE")
  dag.add_dependency("API", "CODE_FE")
  dag.add_dependency("UI", "CODE_FE")
  dag.add_dependency("CODE_BE", "TEST_UNIT")
  dag.add_dependency("CODE_FE", "TEST_UNIT")
  dag.add_dependency("TEST_UNIT", "TEST_INT")
  dag.add_dependency("TEST_INT", "TEST_E2E")
  dag.add_dependency("CODE_BE", "DOC")
  dag.add_dependency("CODE_FE", "DOC")
  dag.add_dependency("TEST_UNIT", "REVIEW")
  dag.add_dependency("DOC", "REVIEW")
  dag.add_dependency("REVIEW", "DEPLOY")
  dag.add_dependency("TEST_E2E", "DEPLOY")

  # Check for cycles and print order
  check_and_print_order(dag)

  # Adding additional dependencies
  dag.add_dependency("REQ", "DB")       # DB now depends on both REQ and DESIGN
  dag.add_dependency("SPEC", "API")     # API now depends on both SPEC and DESIGN
  dag.add_dependency("TEST_INT", "DOC") # DOC now depends on CODE_BE, CODE_FE and TEST_INT
  dag.add_dependency("TEST_INT", "REVIEW") # REVIEW now depends on TEST_UNIT, DOC and TEST_INT

  # Check for cycles and print order
  check_and_print_order(dag)
end

if __FILE__ == $PROGRAM_NAME
  main
end
