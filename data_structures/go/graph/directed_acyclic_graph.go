package main

import (
	"errors"
	"fmt"
	"sort"
)

type DAG struct {
	graph map[string][]string
	tasks map[string]string
}

func NewDAG() *DAG {
	return &DAG{
		graph: make(map[string][]string),
		tasks: make(map[string]string),
	}
}

func (d *DAG) AddTask(taskID, description string) {
	d.tasks[taskID] = description
	if _, exists := d.graph[taskID]; !exists {
		d.graph[taskID] = []string{}
	}
}

func (d *DAG) AddDependency(fromTask, toTask string) error {
	if _, exists := d.tasks[fromTask]; !exists {
		return errors.New("from task does not exist in the DAG")
	}
	if _, exists := d.tasks[toTask]; !exists {
		return errors.New("to task does not exist in the DAG")
	}
	d.graph[fromTask] = append(d.graph[fromTask], toTask)
	return nil
}

func (d *DAG) HasCycle() bool {
	visited := make(map[string]bool)
	recStack := make(map[string]bool)

	var dfs func(task string) bool
	dfs = func(task string) bool {
		visited[task] = true
		recStack[task] = true

		for _, neighbor := range d.graph[task] {
			if !visited[neighbor] {
				if dfs(neighbor) {
					return true
				}
			} else if recStack[neighbor] {
				return true
			}
		}

		recStack[task] = false
		return false
	}

	for task := range d.tasks {
		if !visited[task] {
			if dfs(task) {
				return true
			}
		}
	}
	return false
}

func (d *DAG) TopologicalSort() ([]string, error) {
	if d.HasCycle() {
		return nil, errors.New("DAG contains a cycle, cannot perform topological sort")
	}

	// Compute in-degree for each task
	inDegree := make(map[string]int)
	for task := range d.tasks {
		inDegree[task] = 0
	}
	for _, neighbors := range d.graph {
		for _, neighbor := range neighbors {
			inDegree[neighbor]++
		}
	}

	// Initialize queue with tasks having in-degree 0
	var queue []string
	for task := range d.tasks {
		if inDegree[task] == 0 {
			queue = append(queue, task)
		}
	}
	sort.Strings(queue) // Sort for deterministic output

	var result []string

	for len(queue) > 0 {
		// Pop from front
		current := queue[0]
		queue = queue[1:]
		result = append(result, current)

		// Get neighbors and sort for deterministic order
		neighbors := make([]string, len(d.graph[current]))
		copy(neighbors, d.graph[current])
		sort.Strings(neighbors)

		// Reduce in-degree for neighbors
		for _, neighbor := range neighbors {
			inDegree[neighbor]--
			if inDegree[neighbor] == 0 {
				queue = append(queue, neighbor)
				sort.Strings(queue)
			}
		}
	}

	if len(result) != len(d.tasks) {
		return nil, errors.New("DAG contains a cycle or disconnected components")
	}
	return result, nil
}

func (d *DAG) PrintTasks(taskOrder []string) {
	fmt.Println("Task Execution Order:")
	for _, task := range taskOrder {
		fmt.Printf("%s: %s\n", task, d.tasks[task])
	}
}

func checkAndPrintOrder(dag *DAG) {
	if !dag.HasCycle() {
		fmt.Println("\nNo cycles detected in the DAG.\n")
		taskOrder, err := dag.TopologicalSort()
		if err != nil {
			fmt.Println("Error:", err)
			return
		}
		dag.PrintTasks(taskOrder)

		// Build dependency summary
		var dependencies []string
		for task, neighbors := range dag.graph {
			for _, neighbor := range neighbors {
				dep := fmt.Sprintf("%s (%s) -> %s (%s)", task, dag.tasks[task], neighbor, dag.tasks[neighbor])
				dependencies = append(dependencies, dep)
			}
		}

		// Print dependency summary
		sort.Strings(dependencies)
		fmt.Println("\nDependency Summary:")
		for _, dep := range dependencies {
			fmt.Println(dep)
		}
	} else {
		fmt.Println("\nCycle detected in the DAG.\n")
	}
}

func main() {
	dag := NewDAG()

	// Add tasks
	dag.AddTask("A", "Research topic")
	dag.AddTask("B", "Create slides")
	dag.AddTask("C", "Practice delivery")
	dag.AddTask("D", "Finalize presentation")

	// Add dependencies
	dag.AddDependency("A", "B") // B depends on A
	dag.AddDependency("B", "C") // C depends on B
	dag.AddDependency("B", "D") // D depends on B
	dag.AddDependency("C", "D") // D depends on C

	// Check for cycles and print order
	checkAndPrintOrder(dag)

	// Create a more complex DAG for software development lifecycle
	dag = NewDAG()

	// Add tasks for different phases
	dag.AddTask("REQ", "Requirements gathering")
	dag.AddTask("SPEC", "Write technical specification")
	dag.AddTask("DESIGN", "System architecture design")
	dag.AddTask("DB", "Database schema design")
	dag.AddTask("API", "API design")
	dag.AddTask("UI", "UI/UX design")
	dag.AddTask("CODE_BE", "Backend implementation")
	dag.AddTask("CODE_FE", "Frontend implementation")
	dag.AddTask("TEST_UNIT", "Unit testing")
	dag.AddTask("TEST_INT", "Integration testing")
	dag.AddTask("TEST_E2E", "End-to-end testing")
	dag.AddTask("DOC", "Documentation")
	dag.AddTask("REVIEW", "Code review")
	dag.AddTask("DEPLOY", "Deployment")

	// Add dependencies
	dag.AddDependency("REQ", "SPEC")
	dag.AddDependency("SPEC", "DESIGN")
	dag.AddDependency("DESIGN", "DB")
	dag.AddDependency("DESIGN", "API")
	dag.AddDependency("DESIGN", "UI")
	dag.AddDependency("DB", "CODE_BE")
	dag.AddDependency("API", "CODE_BE")
	dag.AddDependency("API", "CODE_FE")
	dag.AddDependency("UI", "CODE_FE")
	dag.AddDependency("CODE_BE", "TEST_UNIT")
	dag.AddDependency("CODE_FE", "TEST_UNIT")
	dag.AddDependency("TEST_UNIT", "TEST_INT")
	dag.AddDependency("TEST_INT", "TEST_E2E")
	dag.AddDependency("CODE_BE", "DOC")
	dag.AddDependency("CODE_FE", "DOC")
	dag.AddDependency("TEST_UNIT", "REVIEW")
	dag.AddDependency("DOC", "REVIEW")
	dag.AddDependency("REVIEW", "DEPLOY")
	dag.AddDependency("TEST_E2E", "DEPLOY")

	// Check for cycles and print order
	checkAndPrintOrder(dag)

	// Adding additional dependencies
	dag.AddDependency("REQ", "DB")       // DB now depends on both REQ and DESIGN
	dag.AddDependency("SPEC", "API")     // API now depends on both SPEC and DESIGN
	dag.AddDependency("TEST_INT", "DOC") // DOC now depends on CODE_BE, CODE_FE and TEST_INT
	dag.AddDependency("TEST_INT", "REVIEW")

	// Check for cycles and print order
	checkAndPrintOrder(dag)
}
