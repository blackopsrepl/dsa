#include <stdio.h>

/**
 * Simple (linear) search algorithm - O(n)
 * Searches for an item in an array by checking each element sequentially.
 *
 * @param arr Array to search in
 * @param size Size of the array
 * @param target Item to search for
 * @return Index of the item if found, -1 otherwise
 */
int simple_search(int arr[], int size, int target) {
    for (int i = 0; i < size; i++) {
        if (arr[i] == target) {
            return i;
        }
    }
    return -1;
}

int main() {
    int my_list[] = {1, 3, 5, 7, 9, 27, 6, 235, 55};
    int size = sizeof(my_list) / sizeof(my_list[0]);

    // Test case 1: Find element that exists
    int result = simple_search(my_list, size, 6);
    printf("Search for 6: index %d\n", result);
    if (result != 6) {
        printf("FAIL: Expected index 6, got %d\n", result);
        return 1;
    }

    // Test case 2: Find first element
    result = simple_search(my_list, size, 1);
    printf("Search for 1: index %d\n", result);
    if (result != 0) {
        printf("FAIL: Expected index 0, got %d\n", result);
        return 1;
    }

    // Test case 3: Find last element
    result = simple_search(my_list, size, 55);
    printf("Search for 55: index %d\n", result);
    if (result != 8) {
        printf("FAIL: Expected index 8, got %d\n", result);
        return 1;
    }

    // Test case 4: Element not found
    result = simple_search(my_list, size, 100);
    printf("Search for 100: index %d\n", result);
    if (result != -1) {
        printf("FAIL: Expected -1, got %d\n", result);
        return 1;
    }

    // Test case 5: Empty array
    int empty[] = {};
    result = simple_search(empty, 0, 5);
    printf("Search in empty array: index %d\n", result);
    if (result != -1) {
        printf("FAIL: Expected -1, got %d\n", result);
        return 1;
    }

    printf("\nAll tests passed!\n");
    return 0;
}
