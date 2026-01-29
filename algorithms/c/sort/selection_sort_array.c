#include <stdio.h>

/**
 * Find the index of the smallest element in the array starting from `start`.
 *
 * @param arr Array to search
 * @param size Size of the array
 * @param start Starting index for the search
 * @return Index of the smallest element
 */
int find_smallest_index(int arr[], int size, int start) {
    int smallest = arr[start];
    int smallest_index = start;

    for (int i = start + 1; i < size; i++) {
        if (arr[i] < smallest) {
            smallest = arr[i];
            smallest_index = i;
        }
    }

    return smallest_index;
}

/**
 * Selection sort algorithm - O(n^2)
 * Sorts an array in-place by repeatedly finding the minimum element
 * and placing it at the beginning.
 *
 * @param arr Array to sort
 * @param size Size of the array
 */
void selection_sort(int arr[], int size) {
    for (int i = 0; i < size - 1; i++) {
        int smallest_index = find_smallest_index(arr, size, i);

        // Swap arr[i] with arr[smallest_index]
        if (smallest_index != i) {
            int temp = arr[i];
            arr[i] = arr[smallest_index];
            arr[smallest_index] = temp;
        }
    }
}

void print_array(int arr[], int size) {
    printf("[");
    for (int i = 0; i < size; i++) {
        printf("%d", arr[i]);
        if (i < size - 1) {
            printf(", ");
        }
    }
    printf("]\n");
}

int arrays_equal(int a[], int b[], int size) {
    for (int i = 0; i < size; i++) {
        if (a[i] != b[i]) {
            return 0;
        }
    }
    return 1;
}

int main() {
    // Test case 1: Basic unsorted array
    int arr1[] = {5, 3, 6, 2, 10};
    int expected1[] = {2, 3, 5, 6, 10};
    int size1 = sizeof(arr1) / sizeof(arr1[0]);

    printf("Before: ");
    print_array(arr1, size1);
    selection_sort(arr1, size1);
    printf("After:  ");
    print_array(arr1, size1);

    if (!arrays_equal(arr1, expected1, size1)) {
        printf("FAIL: Test case 1\n");
        return 1;
    }

    // Test case 2: Already sorted array
    int arr2[] = {1, 2, 3, 4, 5};
    int expected2[] = {1, 2, 3, 4, 5};
    int size2 = sizeof(arr2) / sizeof(arr2[0]);

    printf("\nBefore: ");
    print_array(arr2, size2);
    selection_sort(arr2, size2);
    printf("After:  ");
    print_array(arr2, size2);

    if (!arrays_equal(arr2, expected2, size2)) {
        printf("FAIL: Test case 2\n");
        return 1;
    }

    // Test case 3: Reverse sorted array
    int arr3[] = {5, 4, 3, 2, 1};
    int expected3[] = {1, 2, 3, 4, 5};
    int size3 = sizeof(arr3) / sizeof(arr3[0]);

    printf("\nBefore: ");
    print_array(arr3, size3);
    selection_sort(arr3, size3);
    printf("After:  ");
    print_array(arr3, size3);

    if (!arrays_equal(arr3, expected3, size3)) {
        printf("FAIL: Test case 3\n");
        return 1;
    }

    // Test case 4: Array with duplicates
    int arr4[] = {3, 1, 4, 1, 5, 9, 2, 6, 5};
    int expected4[] = {1, 1, 2, 3, 4, 5, 5, 6, 9};
    int size4 = sizeof(arr4) / sizeof(arr4[0]);

    printf("\nBefore: ");
    print_array(arr4, size4);
    selection_sort(arr4, size4);
    printf("After:  ");
    print_array(arr4, size4);

    if (!arrays_equal(arr4, expected4, size4)) {
        printf("FAIL: Test case 4\n");
        return 1;
    }

    // Test case 5: Single element array
    int arr5[] = {42};
    int expected5[] = {42};
    int size5 = 1;

    printf("\nBefore: ");
    print_array(arr5, size5);
    selection_sort(arr5, size5);
    printf("After:  ");
    print_array(arr5, size5);

    if (!arrays_equal(arr5, expected5, size5)) {
        printf("FAIL: Test case 5\n");
        return 1;
    }

    printf("\nAll tests passed!\n");
    return 0;
}
