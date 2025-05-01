def find_smallest_in_array(arr):
    # stores the smallest value
    smallest = arr[0]
    
    # stores the index of the smallest value
    smallest_index = 0

    for i in range(1, len(arr)):
        if arr[i] < smallest:
            smallest = arr[i]
            smallest_index = i
    
    return smallest_index


def selection_sort_array(arr):
    new_arr = []

    for i in range(len(arr)):
        # finds the smallest element in the array
        smallest = find_smallest_in_array(arr)

        # adds it to the new array
        new_arr.append(arr.pop(smallest))

    return new_arr


print(selection_sort_array([5, 3, 6, 2, 10]))