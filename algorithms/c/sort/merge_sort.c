#include <stdio.h>
#include <stdlib.h>

/**
 * Merge two sorted arrays into a single sorted array.
 *
 * @param left First sorted array
 * @param left_size Size of left array
 * @param right Second sorted array
 * @param right_size Size of right array
 * @return New merged sorted array (caller must free)
 */
int* merge(int left[], int left_size, int right[], int right_size) {
    int total_size = left_size + right_size;
    int* result = malloc(total_size * sizeof(int));

    int i = 0, j = 0, k = 0;

    // Compare elements from both arrays and add the smaller one
    while (i < left_size && j < right_size) {
        if (left[i] <= right[j]) {
            result[k++] = left[i++];
        } else {
            result[k++] = right[j++];
        }
    }

    // Add remaining elements from left array
    while (i < left_size) {
        result[k++] = left[i++];
    }

    // Add remaining elements from right array
    while (j < right_size) {
        result[k++] = right[j++];
    }

    return result;
}

/**
 * Merge sort algorithm - O(n log n)
 * Divides the array into halves, recursively sorts them, and merges the results.
 *
 * @param arr Array to sort
 * @param size Size of the array
 * @return New sorted array (caller must free)
 */
int* merge_sort(int arr[], int size) {
    // Base case: array of 0 or 1 elements is already sorted
    if (size <= 1) {
        int* result = malloc(size * sizeof(int));
        if (result != NULL && size == 1) {
            result[0] = arr[0];
        }
        return result;
    }

    // Divide the array into two halves
    int mid = size / 2;
    int left_size = mid;
    int right_size = size - mid;

    // Recursively sort both halves
    int* left_sorted = merge_sort(arr, left_size);
    int* right_sorted = merge_sort(arr + mid, right_size);

    // Merge the sorted halves
    int* result = merge(left_sorted, left_size, right_sorted, right_size);

    // Free temporary arrays
    free(left_sorted);
    free(right_sorted);

    return result;
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
    // Test case 1: Basic unsorted array (from Python example)
    int arr1[] = {38, 27, 43, 3, 9, 82, 10};
    int expected1[] = {3, 9, 10, 27, 38, 43, 82};
    int size1 = sizeof(arr1) / sizeof(arr1[0]);

    printf("Before: ");
    print_array(arr1, size1);
    int* sorted1 = merge_sort(arr1, size1);
    printf("After:  ");
    print_array(sorted1, size1);

    if (!arrays_equal(sorted1, expected1, size1)) {
        printf("FAIL: Test case 1\n");
        return 1;
    }
    free(sorted1);

    // Test case 2: Already sorted array
    int arr2[] = {1, 2, 3, 4, 5};
    int expected2[] = {1, 2, 3, 4, 5};
    int size2 = sizeof(arr2) / sizeof(arr2[0]);

    printf("\nBefore: ");
    print_array(arr2, size2);
    int* sorted2 = merge_sort(arr2, size2);
    printf("After:  ");
    print_array(sorted2, size2);

    if (!arrays_equal(sorted2, expected2, size2)) {
        printf("FAIL: Test case 2\n");
        return 1;
    }
    free(sorted2);

    // Test case 3: Reverse sorted array
    int arr3[] = {5, 4, 3, 2, 1};
    int expected3[] = {1, 2, 3, 4, 5};
    int size3 = sizeof(arr3) / sizeof(arr3[0]);

    printf("\nBefore: ");
    print_array(arr3, size3);
    int* sorted3 = merge_sort(arr3, size3);
    printf("After:  ");
    print_array(sorted3, size3);

    if (!arrays_equal(sorted3, expected3, size3)) {
        printf("FAIL: Test case 3\n");
        return 1;
    }
    free(sorted3);

    // Test case 4: Array with duplicates
    int arr4[] = {3, 1, 4, 1, 5, 9, 2, 6, 5};
    int expected4[] = {1, 1, 2, 3, 4, 5, 5, 6, 9};
    int size4 = sizeof(arr4) / sizeof(arr4[0]);

    printf("\nBefore: ");
    print_array(arr4, size4);
    int* sorted4 = merge_sort(arr4, size4);
    printf("After:  ");
    print_array(sorted4, size4);

    if (!arrays_equal(sorted4, expected4, size4)) {
        printf("FAIL: Test case 4\n");
        return 1;
    }
    free(sorted4);

    // Test case 5: Single element array
    int arr5[] = {42};
    int expected5[] = {42};
    int size5 = 1;

    printf("\nBefore: ");
    print_array(arr5, size5);
    int* sorted5 = merge_sort(arr5, size5);
    printf("After:  ");
    print_array(sorted5, size5);

    if (!arrays_equal(sorted5, expected5, size5)) {
        printf("FAIL: Test case 5\n");
        return 1;
    }
    free(sorted5);

    // Test case 6: Empty array
    int* arr6 = NULL;
    int size6 = 0;

    printf("\nBefore: []\n");
    int* sorted6 = merge_sort(arr6, size6);
    printf("After:  []\n");
    free(sorted6);

    printf("\nAll tests passed!\n");
    return 0;
}
