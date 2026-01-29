#include <stdio.h>

/**
 * Binary search algorithm - O(log n)
 * Searches for an item in a SORTED array by repeatedly dividing the search
 * interval in half.
 *
 * @param arr Sorted array to search in
 * @param size Size of the array
 * @param target Item to search for
 * @return Index of the item if found, -1 otherwise
 */
int binary_search(int arr[], int size, int target) {
    int low = 0;
    int high = size - 1;

    while (low <= high) {
        int mid = (low + high) / 2;
        int guess = arr[mid];

        if (guess == target) {
            return mid;
        }

        if (guess > target) {
            high = mid - 1;
        } else {
            low = mid + 1;
        }
    }

    return -1;
}

int main() {
    int my_list[] = {1, 3, 5, 7, 9, 27, 29, 235, 4567};
    int size = sizeof(my_list) / sizeof(my_list[0]);

    // Test case 1: Find element in middle
    int result = binary_search(my_list, size, 27);
    printf("Search for 27: index %d\n", result);
    if (result != 5) {
        printf("FAIL: Expected index 5, got %d\n", result);
        return 1;
    }

    // Test case 2: Find first element
    result = binary_search(my_list, size, 1);
    printf("Search for 1: index %d\n", result);
    if (result != 0) {
        printf("FAIL: Expected index 0, got %d\n", result);
        return 1;
    }

    // Test case 3: Find last element
    result = binary_search(my_list, size, 4567);
    printf("Search for 4567: index %d\n", result);
    if (result != 8) {
        printf("FAIL: Expected index 8, got %d\n", result);
        return 1;
    }

    // Test case 4: Element not found
    result = binary_search(my_list, size, 100);
    printf("Search for 100: index %d\n", result);
    if (result != -1) {
        printf("FAIL: Expected -1, got %d\n", result);
        return 1;
    }

    // Test case 5: Empty array
    int empty[] = {};
    result = binary_search(empty, 0, 5);
    printf("Search in empty array: index %d\n", result);
    if (result != -1) {
        printf("FAIL: Expected -1, got %d\n", result);
        return 1;
    }

    // Test case 6: Single element array - found
    int single[] = {42};
    result = binary_search(single, 1, 42);
    printf("Search for 42 in single element array: index %d\n", result);
    if (result != 0) {
        printf("FAIL: Expected index 0, got %d\n", result);
        return 1;
    }

    // Test case 7: Single element array - not found
    result = binary_search(single, 1, 10);
    printf("Search for 10 in single element array: index %d\n", result);
    if (result != -1) {
        printf("FAIL: Expected -1, got %d\n", result);
        return 1;
    }

    printf("\nAll tests passed!\n");
    return 0;
}
