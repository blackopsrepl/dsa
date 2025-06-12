from collections import defaultdict, deque

# DAG example implementation for task dependency management
# This DAG (Directed Acyclic Graph) implementation manages task dependencies:

# The DAG class uses:
# - defaultdict to store the graph as an adjacency list (task -> dependent tasks)
# - Dictionary to store task descriptions
# - DFS-based cycle detection
# - Topological sort using Kahn's algorithm with in-degree tracking

# Key methods:
# add_task(task_id, description) - Adds a new task to the DAG
# add_dependency(from_task, to_task) - Creates dependency between tasks
# has_cycle() - Checks for cycles using DFS traversal
# topological_sort() - Returns tasks in dependency order using Kahn's algorithm
# print_tasks(task_order) - Prints tasks and descriptions in given order

# Example usage creates a presentation preparation DAG:
# A (Research) -> B (Slides) -> C (Practice) -> D (Finalize)
#                      |_________________^

class DAG:
    def __init__(self):
        # Adjacency list to store the graph (task -> list of dependent tasks)
        self.graph = defaultdict(list)

        # Store task descriptions
        self.tasks = {}

    def add_task(self, task_id, description):
        """Add a task to the DAG."""
        self.tasks[task_id] = description

    def add_dependency(self, from_task, to_task):
        """Add a dependency (edge) from from_task to to_task."""
        if from_task not in self.tasks or to_task not in self.tasks:
            raise ValueError("Both tasks must exist in the DAG.")
        self.graph[from_task].append(to_task)

    def has_cycle(self):
        """Check for cycles using DFS."""
        visited = set()
        rec_stack = set()

        def dfs(task):
            visited.add(task)
            rec_stack.add(task)
            for neighbor in self.graph[task]:
                if neighbor not in visited:
                    if dfs(neighbor):
                        return True
                elif neighbor in rec_stack:
                    return True
            rec_stack.remove(task)
            return False

        for task in self.tasks:
            if task not in visited:
                if dfs(task):
                    return True
        return False

    def topological_sort(self):
        """Perform topological sort to get task execution order."""
        if self.has_cycle():
            raise ValueError("DAG contains a cycle, cannot perform topological sort.")

        # Compute in-degree for each task
        in_degree = {task: 0 for task in self.tasks}
        for task in self.graph:
            for neighbor in self.graph[task]:
                in_degree[neighbor] += 1

        # Initialize queue with tasks having in-degree 0
        queue = deque([task for task in self.tasks if in_degree[task] == 0])
        result = []

        while queue:
            current = queue.popleft()
            result.append(current)
            # Reduce in-degree for neighbors
            for neighbor in self.graph[current]:
                in_degree[neighbor] -= 1
                if in_degree[neighbor] == 0:
                    queue.append(neighbor)

        if len(result) != len(self.tasks):
            raise ValueError("DAG contains a cycle or disconnected components.")
        return result

    def print_tasks(self, task_order):
        """Print tasks in the given order with descriptions."""
        print("Task Execution Order:")
        for task in task_order:
            print(f"{task}: {self.tasks[task]}")


def check_and_print_order(dag):
    """Check for cycles and print task order."""
    if not dag.has_cycle():
        print("\n\nNo cycles detected in the DAG.\n\n")
        task_order = dag.topological_sort()
        dag.print_tasks(task_order)
        
        # Build dependency summary
        dependencies = []
        for task in dag.graph:
            for neighbor in dag.graph[task]:
                dependencies.append(f"{task} ({dag.tasks[task]}) -> {neighbor} ({dag.tasks[neighbor]})")
                
        # Print dependency summary
        print("\nDependency Summary:")
        for dep in sorted(dependencies):
            print(dep)
            
    else:
        print("\n\nCycle detected in the DAG.\n\n")


def main():
    dag = DAG()

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
    dag = DAG()

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
    dag.add_dependency("REQ", "DB")  # DB now depends on both REQ and DESIGN
    dag.add_dependency("SPEC", "API") # API now depends on both SPEC and DESIGN
    dag.add_dependency("TEST_INT", "DOC") # DOC now depends on CODE_BE, CODE_FE and TEST_INT
    dag.add_dependency("TEST_INT", "REVIEW") # REVIEW now depends on TEST_UNIT, DOC and TEST_INT

    # Check for cycles and print order
    check_and_print_order(dag)

if __name__ == "__main__":
    main()
