def merge_sort(arr):
    """
    Sort an array using the merge sort algorithm.
    
    Args:
        arr: List to be sorted
        
    Returns:
        A new sorted list
    """
    # Base case: array of length 0 or 1 is already sorted
    if len(arr) <= 1:
        return arr
    
    # Divide the array into two halves
    mid = len(arr) // 2
    left_half = arr[:mid]
    right_half = arr[mid:]
    
    # Recursively sort both halves
    left_half = merge_sort(left_half)
    right_half = merge_sort(right_half)
    
    # Merge the sorted halves
    return merge(left_half, right_half)

def merge(left, right):
    """
    Merge two sorted arrays into a single sorted array.
    
    Args:
        left: First sorted array
        right: Second sorted array
        
    Returns:
        A new sorted array containing all elements from left and right
    """
    result = []
    i = j = 0
    
    # Compare elements from both arrays and add the smaller one to the result
    while i < len(left) and j < len(right):
        if left[i] <= right[j]:
            result.append(left[i])
            i += 1
        else:
            result.append(right[j])
            j += 1
    
    # Add any remaining elements from both arrays
    result.extend(left[i:])
    result.extend(right[j:])
    
    return result

# Example usage
if __name__ == "__main__":
    arr = [38, 27, 43, 3, 9, 82, 10]
    sorted_arr = merge_sort(arr)
    print({sorted_arr})
    