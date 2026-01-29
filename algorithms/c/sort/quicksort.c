#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/**
 * Quicksort algorithm - O(n log n) average case
 * This implementation uses the functional approach similar to the Python version,
 * creating new arrays rather than sorting in-place.
 *
 * @param arr Array to sort
 * @param size Size of the array
 * @param result_size Pointer to store the resulting array size
 * @return New sorted array (caller must free)
 */
int* quicksort(int arr[], int size, int* result_size) {
    *result_size = size;

    // Base case: array of 0 or 1 elements is already sorted
    if (size <= 1) {
        int* result = malloc(size * sizeof(int));
        if (result != NULL && size == 1) {
            result[0] = arr[0];
        }
        return result;
    }

    // Choose pivot as middle element
    int pivot = arr[size / 2];

    // Count elements for left, middle, right partitions
    int left_count = 0, middle_count = 0, right_count = 0;
    for (int i = 0; i < size; i++) {
        if (arr[i] < pivot) left_count++;
        else if (arr[i] == pivot) middle_count++;
        else right_count++;
    }

    // Allocate partition arrays
    int* left = malloc(left_count * sizeof(int));
    int* middle = malloc(middle_count * sizeof(int));
    int* right = malloc(right_count * sizeof(int));

    // Fill partition arrays
    int li = 0, mi = 0, ri = 0;
    for (int i = 0; i < size; i++) {
        if (arr[i] < pivot) left[li++] = arr[i];
        else if (arr[i] == pivot) middle[mi++] = arr[i];
        else right[ri++] = arr[i];
    }

    // Recursively sort left and right partitions
    int sorted_left_size, sorted_right_size;
    int* sorted_left = quicksort(left, left_count, &sorted_left_size);
    int* sorted_right = quicksort(right, right_count, &sorted_right_size);

    // Combine results: sorted_left + middle + sorted_right
    int total_size = sorted_left_size + middle_count + sorted_right_size;
    int* result = malloc(total_size * sizeof(int));

    int idx = 0;
    for (int i = 0; i < sorted_left_size; i++) result[idx++] = sorted_left[i];
    for (int i = 0; i < middle_count; i++) result[idx++] = middle[i];
    for (int i = 0; i < sorted_right_size; i++) result[idx++] = sorted_right[i];

    // Free temporary arrays
    free(left);
    free(middle);
    free(right);
    free(sorted_left);
    free(sorted_right);

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
    int arr1[] = {3, 6, 8, 10, 1, 2, 1};
    int expected1[] = {1, 1, 2, 3, 6, 8, 10};
    int size1 = sizeof(arr1) / sizeof(arr1[0]);
    int result_size1;

    printf("Before: ");
    print_array(arr1, size1);
    int* sorted1 = quicksort(arr1, size1, &result_size1);
    printf("After:  ");
    print_array(sorted1, result_size1);

    if (!arrays_equal(sorted1, expected1, size1)) {
        printf("FAIL: Test case 1\n");
        return 1;
    }
    free(sorted1);

    // Test case 2: Already sorted array
    int arr2[] = {1, 2, 3, 4, 5};
    int expected2[] = {1, 2, 3, 4, 5};
    int size2 = sizeof(arr2) / sizeof(arr2[0]);
    int result_size2;

    printf("\nBefore: ");
    print_array(arr2, size2);
    int* sorted2 = quicksort(arr2, size2, &result_size2);
    printf("After:  ");
    print_array(sorted2, result_size2);

    if (!arrays_equal(sorted2, expected2, size2)) {
        printf("FAIL: Test case 2\n");
        return 1;
    }
    free(sorted2);

    // Test case 3: Reverse sorted array
    int arr3[] = {5, 4, 3, 2, 1};
    int expected3[] = {1, 2, 3, 4, 5};
    int size3 = sizeof(arr3) / sizeof(arr3[0]);
    int result_size3;

    printf("\nBefore: ");
    print_array(arr3, size3);
    int* sorted3 = quicksort(arr3, size3, &result_size3);
    printf("After:  ");
    print_array(sorted3, result_size3);

    if (!arrays_equal(sorted3, expected3, size3)) {
        printf("FAIL: Test case 3\n");
        return 1;
    }
    free(sorted3);

    // Test case 4: Array with all same elements
    int arr4[] = {7, 7, 7, 7};
    int expected4[] = {7, 7, 7, 7};
    int size4 = sizeof(arr4) / sizeof(arr4[0]);
    int result_size4;

    printf("\nBefore: ");
    print_array(arr4, size4);
    int* sorted4 = quicksort(arr4, size4, &result_size4);
    printf("After:  ");
    print_array(sorted4, result_size4);

    if (!arrays_equal(sorted4, expected4, size4)) {
        printf("FAIL: Test case 4\n");
        return 1;
    }
    free(sorted4);

    // Test case 5: Single element array
    int arr5[] = {42};
    int expected5[] = {42};
    int size5 = 1;
    int result_size5;

    printf("\nBefore: ");
    print_array(arr5, size5);
    int* sorted5 = quicksort(arr5, size5, &result_size5);
    printf("After:  ");
    print_array(sorted5, result_size5);

    if (!arrays_equal(sorted5, expected5, size5)) {
        printf("FAIL: Test case 5\n");
        return 1;
    }
    free(sorted5);

    // Test case 6: Empty array
    int* arr6 = NULL;
    int size6 = 0;
    int result_size6;

    printf("\nBefore: []\n");
    int* sorted6 = quicksort(arr6, size6, &result_size6);
    printf("After:  []\n");

    if (result_size6 != 0) {
        printf("FAIL: Test case 6\n");
        return 1;
    }
    free(sorted6);

    printf("\nAll tests passed!\n");
    return 0;
}
