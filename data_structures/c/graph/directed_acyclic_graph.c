#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

/**
 * Directed Acyclic Graph (DAG) Implementation
 *
 * A DAG for task dependency management with:
 * - Task storage with IDs and descriptions
 * - Adjacency matrix for edge representation
 * - DFS-based cycle detection
 * - Topological sort using Kahn's algorithm
 */

#define MAX_TASKS 100
#define MAX_ID_LEN 32
#define MAX_DESC_LEN 256

typedef struct DAG {
    char task_ids[MAX_TASKS][MAX_ID_LEN];
    char descriptions[MAX_TASKS][MAX_DESC_LEN];
    int adjacency[MAX_TASKS][MAX_TASKS];  // adjacency[i][j] = 1 if edge from i to j
    int task_count;
} DAG;

void dag_init(DAG* dag) {
    dag->task_count = 0;
    memset(dag->adjacency, 0, sizeof(dag->adjacency));
}

int find_task_index(DAG* dag, const char* task_id) {
    for (int i = 0; i < dag->task_count; i++) {
        if (strcmp(dag->task_ids[i], task_id) == 0) {
            return i;
        }
    }
    return -1;
}

bool add_task(DAG* dag, const char* task_id, const char* description) {
    if (dag->task_count >= MAX_TASKS) {
        return false;
    }

    if (find_task_index(dag, task_id) >= 0) {
        return false;  // Task already exists
    }

    strncpy(dag->task_ids[dag->task_count], task_id, MAX_ID_LEN - 1);
    dag->task_ids[dag->task_count][MAX_ID_LEN - 1] = '\0';

    strncpy(dag->descriptions[dag->task_count], description, MAX_DESC_LEN - 1);
    dag->descriptions[dag->task_count][MAX_DESC_LEN - 1] = '\0';

    dag->task_count++;
    return true;
}

bool add_dependency(DAG* dag, const char* from_task, const char* to_task) {
    int from_idx = find_task_index(dag, from_task);
    int to_idx = find_task_index(dag, to_task);

    if (from_idx < 0 || to_idx < 0) {
        return false;  // Both tasks must exist
    }

    dag->adjacency[from_idx][to_idx] = 1;
    return true;
}

// DFS helper for cycle detection
static bool dfs_has_cycle(DAG* dag, int node, bool visited[], bool rec_stack[]) {
    visited[node] = true;
    rec_stack[node] = true;

    for (int i = 0; i < dag->task_count; i++) {
        if (dag->adjacency[node][i]) {
            if (!visited[i]) {
                if (dfs_has_cycle(dag, i, visited, rec_stack)) {
                    return true;
                }
            } else if (rec_stack[i]) {
                return true;
            }
        }
    }

    rec_stack[node] = false;
    return false;
}

bool has_cycle(DAG* dag) {
    bool visited[MAX_TASKS] = {false};
    bool rec_stack[MAX_TASKS] = {false};

    for (int i = 0; i < dag->task_count; i++) {
        if (!visited[i]) {
            if (dfs_has_cycle(dag, i, visited, rec_stack)) {
                return true;
            }
        }
    }
    return false;
}

/**
 * Perform topological sort using Kahn's algorithm.
 *
 * @param dag The DAG
 * @param result Array to store result task indices (must be at least task_count size)
 * @param result_size Pointer to store the number of tasks in result
 * @return true if successful, false if DAG contains a cycle
 */
bool topological_sort(DAG* dag, int result[], int* result_size) {
    if (has_cycle(dag)) {
        *result_size = 0;
        return false;
    }

    int in_degree[MAX_TASKS] = {0};
    int queue[MAX_TASKS];
    int queue_front = 0, queue_back = 0;

    // Compute in-degree for each task
    for (int i = 0; i < dag->task_count; i++) {
        for (int j = 0; j < dag->task_count; j++) {
            if (dag->adjacency[j][i]) {
                in_degree[i]++;
            }
        }
    }

    // Initialize queue with tasks having in-degree 0
    for (int i = 0; i < dag->task_count; i++) {
        if (in_degree[i] == 0) {
            queue[queue_back++] = i;
        }
    }

    *result_size = 0;

    while (queue_front < queue_back) {
        int current = queue[queue_front++];
        result[(*result_size)++] = current;

        // Reduce in-degree for neighbors
        for (int i = 0; i < dag->task_count; i++) {
            if (dag->adjacency[current][i]) {
                in_degree[i]--;
                if (in_degree[i] == 0) {
                    queue[queue_back++] = i;
                }
            }
        }
    }

    return *result_size == dag->task_count;
}

void print_tasks(DAG* dag, int task_order[], int size) {
    printf("Task Execution Order:\n");
    for (int i = 0; i < size; i++) {
        int idx = task_order[i];
        printf("%s: %s\n", dag->task_ids[idx], dag->descriptions[idx]);
    }
}

void print_dependency_summary(DAG* dag) {
    printf("\nDependency Summary:\n");
    for (int i = 0; i < dag->task_count; i++) {
        for (int j = 0; j < dag->task_count; j++) {
            if (dag->adjacency[i][j]) {
                printf("%s (%s) -> %s (%s)\n",
                       dag->task_ids[i], dag->descriptions[i],
                       dag->task_ids[j], dag->descriptions[j]);
            }
        }
    }
}

void free_dag(DAG* dag) {
    dag->task_count = 0;
    memset(dag->adjacency, 0, sizeof(dag->adjacency));
}

int main() {
    DAG dag;
    int result[MAX_TASKS];
    int result_size;

    // Test case 1: Simple presentation preparation DAG
    printf("=== Test Case 1: Presentation Preparation ===\n");
    dag_init(&dag);

    add_task(&dag, "A", "Research topic");
    add_task(&dag, "B", "Create slides");
    add_task(&dag, "C", "Practice delivery");
    add_task(&dag, "D", "Finalize presentation");

    add_dependency(&dag, "A", "B");  // B depends on A
    add_dependency(&dag, "B", "C");  // C depends on B
    add_dependency(&dag, "B", "D");  // D depends on B
    add_dependency(&dag, "C", "D");  // D depends on C

    if (!has_cycle(&dag)) {
        printf("No cycles detected in the DAG.\n\n");
        if (topological_sort(&dag, result, &result_size)) {
            print_tasks(&dag, result, result_size);
            print_dependency_summary(&dag);

            // Verify A is first
            if (strcmp(dag.task_ids[result[0]], "A") != 0) {
                printf("FAIL: A should be first\n");
                return 1;
            }
            // Verify D is last
            if (strcmp(dag.task_ids[result[result_size - 1]], "D") != 0) {
                printf("FAIL: D should be last\n");
                return 1;
            }
        }
    } else {
        printf("FAIL: Unexpected cycle detected\n");
        return 1;
    }

    // Test case 2: Cyclic graph should detect cycle
    printf("\n=== Test Case 2: Cyclic Graph Detection ===\n");
    dag_init(&dag);

    add_task(&dag, "X", "Task X");
    add_task(&dag, "Y", "Task Y");
    add_task(&dag, "Z", "Task Z");

    add_dependency(&dag, "X", "Y");
    add_dependency(&dag, "Y", "Z");
    add_dependency(&dag, "Z", "X");  // Creates cycle

    if (has_cycle(&dag)) {
        printf("Correctly detected cycle in the DAG.\n");
    } else {
        printf("FAIL: Should have detected cycle\n");
        return 1;
    }

    // Test case 3: Empty DAG
    printf("\n=== Test Case 3: Empty DAG ===\n");
    dag_init(&dag);

    if (!has_cycle(&dag)) {
        printf("No cycles in empty DAG (correct).\n");
        if (topological_sort(&dag, result, &result_size)) {
            printf("Empty DAG sorted successfully with %d tasks.\n", result_size);
            if (result_size != 0) {
                printf("FAIL: Expected 0 tasks\n");
                return 1;
            }
        }
    }

    // Test case 4: Software development lifecycle DAG
    printf("\n=== Test Case 4: Software Development Lifecycle ===\n");
    dag_init(&dag);

    add_task(&dag, "REQ", "Requirements gathering");
    add_task(&dag, "SPEC", "Write technical specification");
    add_task(&dag, "DESIGN", "System architecture design");
    add_task(&dag, "DB", "Database schema design");
    add_task(&dag, "API", "API design");
    add_task(&dag, "UI", "UI/UX design");
    add_task(&dag, "CODE_BE", "Backend implementation");
    add_task(&dag, "CODE_FE", "Frontend implementation");
    add_task(&dag, "TEST", "Testing");
    add_task(&dag, "DEPLOY", "Deployment");

    add_dependency(&dag, "REQ", "SPEC");
    add_dependency(&dag, "SPEC", "DESIGN");
    add_dependency(&dag, "DESIGN", "DB");
    add_dependency(&dag, "DESIGN", "API");
    add_dependency(&dag, "DESIGN", "UI");
    add_dependency(&dag, "DB", "CODE_BE");
    add_dependency(&dag, "API", "CODE_BE");
    add_dependency(&dag, "API", "CODE_FE");
    add_dependency(&dag, "UI", "CODE_FE");
    add_dependency(&dag, "CODE_BE", "TEST");
    add_dependency(&dag, "CODE_FE", "TEST");
    add_dependency(&dag, "TEST", "DEPLOY");

    if (!has_cycle(&dag)) {
        printf("No cycles detected in the DAG.\n\n");
        if (topological_sort(&dag, result, &result_size)) {
            print_tasks(&dag, result, result_size);

            // Verify REQ is first and DEPLOY is last
            if (strcmp(dag.task_ids[result[0]], "REQ") != 0) {
                printf("FAIL: REQ should be first\n");
                return 1;
            }
            if (strcmp(dag.task_ids[result[result_size - 1]], "DEPLOY") != 0) {
                printf("FAIL: DEPLOY should be last\n");
                return 1;
            }
            printf("\nVerified: REQ is first, DEPLOY is last.\n");
        }
    } else {
        printf("FAIL: Unexpected cycle detected\n");
        return 1;
    }

    // Test case 5: Single task
    printf("\n=== Test Case 5: Single Task ===\n");
    dag_init(&dag);

    add_task(&dag, "SOLO", "Single task");

    if (topological_sort(&dag, result, &result_size)) {
        printf("Single task sorted: %s\n", dag.task_ids[result[0]]);
        if (result_size != 1) {
            printf("FAIL: Expected 1 task\n");
            return 1;
        }
    }

    // Test case 6: Adding duplicate task should fail
    printf("\n=== Test Case 6: Duplicate Task Detection ===\n");
    dag_init(&dag);

    add_task(&dag, "A", "First A");
    if (add_task(&dag, "A", "Duplicate A")) {
        printf("FAIL: Should not allow duplicate task ID\n");
        return 1;
    }
    printf("Correctly rejected duplicate task ID.\n");

    // Test case 7: Adding dependency with non-existent task should fail
    printf("\n=== Test Case 7: Invalid Dependency Detection ===\n");
    dag_init(&dag);

    add_task(&dag, "A", "Task A");
    if (add_dependency(&dag, "A", "NONEXISTENT")) {
        printf("FAIL: Should not allow dependency to non-existent task\n");
        return 1;
    }
    printf("Correctly rejected dependency to non-existent task.\n");

    printf("\n=== All tests passed! ===\n");
    return 0;
}
