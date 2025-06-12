from collections import deque


def topological_sort_graph(graph, nodes):
    """
    Generic implementation of topological sort for any directed graph.
    Args:
        graph: Dict mapping nodes to lists of neighbors
        nodes: Collection of all nodes in the graph
    Returns:
        List of nodes in topological order
    Raises:
        ValueError if graph contains cycles
    """
    # Compute in-degree for each node
    in_degree = {node: 0 for node in nodes}
    for node in graph:
        for neighbor in graph[node]:
            in_degree[neighbor] += 1

    # Initialize queue with nodes having in-degree 0
    queue = deque([node for node in nodes if in_degree[node] == 0])
    result = []

    while queue:
        current = queue.popleft()
        result.append(current)
        # Reduce in-degree for neighbors
        for neighbor in graph[current]:
            in_degree[neighbor] -= 1
            if in_degree[neighbor] == 0:
                queue.append(neighbor)

    if len(result) != len(nodes):
        raise ValueError("Graph contains a cycle or disconnected components.")
    return result


def test_topological_sort():
    ### Test case 1: Simple linear graph ###
    graph1 = {
        'A': ['B'],
        'B': ['C'], 
        'C': []
    }
    nodes1 = ['A', 'B', 'C']
    
    print("Test case 1:")
    print(f"Graph: {graph1}")
    print(f"Nodes: {nodes1}")
    result1 = topological_sort_graph(graph1, nodes1)
    print(f"Result: {result1}\n")
    assert result1 == ['A', 'B', 'C']

    ### Test case 2: Diamond shaped graph ###
    graph2 = {
        'A': ['B', 'C'],
        'B': ['D'],
        'C': ['D'],
        'D': []
    }
    nodes2 = ['A', 'B', 'C', 'D']

    print("Test case 2:")
    print(f"Graph: {graph2}")
    print(f"Nodes: {nodes2}")
    result2 = topological_sort_graph(graph2, nodes2)
    print(f"Result: {result2}\n")

    ### Check A is first and D is last ###
    assert result2[0] == 'A'
    assert result2[-1] == 'D'

    ### Check B and C are in middle (order can vary) ###
    assert set(result2[1:3]) == {'B', 'C'}

    ### Test case 3: Empty graph ###
    graph3 = {}
    nodes3 = []
    
    print("Test case 3:")
    print(f"Graph: {graph3}")
    print(f"Nodes: {nodes3}")
    result3 = topological_sort_graph(graph3, nodes3)
    print(f"Result: {result3}\n")
    assert result3 == []

    ### Test case 4: Graph with cycle should raise error ###
    graph4 = {
        'A': ['B'],
        'B': ['C'],
        'C': ['A']
    }
    nodes4 = ['A', 'B', 'C']

    print("Test case 4:")
    print(f"Graph: {graph4}")
    print(f"Nodes: {nodes4}")
    try:
        result4 = topological_sort_graph(graph4, nodes4)
        assert False, "Expected ValueError for cyclic graph"
    except ValueError as e:
        print(f"Caught expected ValueError: {str(e)}\n")

    print("All test cases passed!")



test_topological_sort()
