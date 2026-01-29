#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_NODES 100
#define MAX_ID_LEN 32

/**
 * Topological sort using Kahn's algorithm - O(V+E)
 * For a directed graph represented as adjacency list.
 */

typedef struct {
    char nodes[MAX_NODES][MAX_ID_LEN];
    int adjacency[MAX_NODES][MAX_NODES];  // adjacency[i][j] = 1 if edge from i to j
    int node_count;
} Graph;

void graph_init(Graph* g) {
    g->node_count = 0;
    memset(g->adjacency, 0, sizeof(g->adjacency));
}

int find_node_index(Graph* g, const char* id) {
    for (int i = 0; i < g->node_count; i++) {
        if (strcmp(g->nodes[i], id) == 0) {
            return i;
        }
    }
    return -1;
}

int add_node(Graph* g, const char* id) {
    int existing = find_node_index(g, id);
    if (existing >= 0) {
        return existing;
    }

    if (g->node_count >= MAX_NODES) {
        return -1;
    }

    strncpy(g->nodes[g->node_count], id, MAX_ID_LEN - 1);
    g->nodes[g->node_count][MAX_ID_LEN - 1] = '\0';
    return g->node_count++;
}

bool add_edge(Graph* g, const char* from, const char* to) {
    int from_idx = find_node_index(g, from);
    int to_idx = find_node_index(g, to);

    if (from_idx < 0 || to_idx < 0) {
        return false;
    }

    g->adjacency[from_idx][to_idx] = 1;
    return true;
}

/**
 * Perform topological sort using Kahn's algorithm.
 *
 * @param g The graph
 * @param result Array to store result node indices (must be at least node_count size)
 * @param result_size Pointer to store the number of nodes in result
 * @return true if successful, false if graph contains a cycle
 */
bool topological_sort(Graph* g, int result[], int* result_size) {
    int in_degree[MAX_NODES] = {0};
    int queue[MAX_NODES];
    int queue_front = 0, queue_back = 0;

    // Compute in-degree for each node
    for (int i = 0; i < g->node_count; i++) {
        for (int j = 0; j < g->node_count; j++) {
            if (g->adjacency[j][i]) {
                in_degree[i]++;
            }
        }
    }

    // Initialize queue with nodes having in-degree 0
    for (int i = 0; i < g->node_count; i++) {
        if (in_degree[i] == 0) {
            queue[queue_back++] = i;
        }
    }

    *result_size = 0;

    while (queue_front < queue_back) {
        int current = queue[queue_front++];
        result[(*result_size)++] = current;

        // Reduce in-degree for neighbors
        for (int i = 0; i < g->node_count; i++) {
            if (g->adjacency[current][i]) {
                in_degree[i]--;
                if (in_degree[i] == 0) {
                    queue[queue_back++] = i;
                }
            }
        }
    }

    // Check if all nodes are in result (no cycle)
    return *result_size == g->node_count;
}

void print_result(Graph* g, int result[], int size) {
    printf("[");
    for (int i = 0; i < size; i++) {
        printf("%s", g->nodes[result[i]]);
        if (i < size - 1) {
            printf(", ");
        }
    }
    printf("]\n");
}

int main() {
    Graph g;
    int result[MAX_NODES];
    int result_size;

    // Test case 1: Simple linear graph A -> B -> C
    printf("Test case 1: Linear graph A -> B -> C\n");
    graph_init(&g);
    add_node(&g, "A");
    add_node(&g, "B");
    add_node(&g, "C");
    add_edge(&g, "A", "B");
    add_edge(&g, "B", "C");

    if (topological_sort(&g, result, &result_size)) {
        printf("Result: ");
        print_result(&g, result, result_size);

        // Verify A comes before B, B comes before C
        int a_pos = -1, b_pos = -1, c_pos = -1;
        for (int i = 0; i < result_size; i++) {
            if (strcmp(g.nodes[result[i]], "A") == 0) a_pos = i;
            if (strcmp(g.nodes[result[i]], "B") == 0) b_pos = i;
            if (strcmp(g.nodes[result[i]], "C") == 0) c_pos = i;
        }
        if (a_pos >= b_pos || b_pos >= c_pos) {
            printf("FAIL: Order violation\n");
            return 1;
        }
    } else {
        printf("FAIL: Unexpected cycle detected\n");
        return 1;
    }

    // Test case 2: Diamond shaped graph
    printf("\nTest case 2: Diamond graph A -> {B, C} -> D\n");
    graph_init(&g);
    add_node(&g, "A");
    add_node(&g, "B");
    add_node(&g, "C");
    add_node(&g, "D");
    add_edge(&g, "A", "B");
    add_edge(&g, "A", "C");
    add_edge(&g, "B", "D");
    add_edge(&g, "C", "D");

    if (topological_sort(&g, result, &result_size)) {
        printf("Result: ");
        print_result(&g, result, result_size);

        // Verify A is first and D is last
        if (strcmp(g.nodes[result[0]], "A") != 0) {
            printf("FAIL: A should be first\n");
            return 1;
        }
        if (strcmp(g.nodes[result[result_size - 1]], "D") != 0) {
            printf("FAIL: D should be last\n");
            return 1;
        }
    } else {
        printf("FAIL: Unexpected cycle detected\n");
        return 1;
    }

    // Test case 3: Empty graph
    printf("\nTest case 3: Empty graph\n");
    graph_init(&g);

    if (topological_sort(&g, result, &result_size)) {
        printf("Result: ");
        print_result(&g, result, result_size);
        if (result_size != 0) {
            printf("FAIL: Expected empty result\n");
            return 1;
        }
    } else {
        printf("FAIL: Unexpected cycle in empty graph\n");
        return 1;
    }

    // Test case 4: Graph with cycle should fail
    printf("\nTest case 4: Cyclic graph A -> B -> C -> A\n");
    graph_init(&g);
    add_node(&g, "A");
    add_node(&g, "B");
    add_node(&g, "C");
    add_edge(&g, "A", "B");
    add_edge(&g, "B", "C");
    add_edge(&g, "C", "A");

    if (topological_sort(&g, result, &result_size)) {
        printf("FAIL: Should have detected cycle\n");
        return 1;
    } else {
        printf("Correctly detected cycle!\n");
    }

    // Test case 5: Single node
    printf("\nTest case 5: Single node\n");
    graph_init(&g);
    add_node(&g, "X");

    if (topological_sort(&g, result, &result_size)) {
        printf("Result: ");
        print_result(&g, result, result_size);
        if (result_size != 1 || strcmp(g.nodes[result[0]], "X") != 0) {
            printf("FAIL: Expected single node X\n");
            return 1;
        }
    } else {
        printf("FAIL: Unexpected cycle\n");
        return 1;
    }

    // Test case 6: Disconnected nodes
    printf("\nTest case 6: Disconnected nodes A, B, C (no edges)\n");
    graph_init(&g);
    add_node(&g, "A");
    add_node(&g, "B");
    add_node(&g, "C");

    if (topological_sort(&g, result, &result_size)) {
        printf("Result: ");
        print_result(&g, result, result_size);
        if (result_size != 3) {
            printf("FAIL: Expected 3 nodes\n");
            return 1;
        }
    } else {
        printf("FAIL: Unexpected cycle\n");
        return 1;
    }

    printf("\nAll tests passed!\n");
    return 0;
}
