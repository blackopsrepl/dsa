def binary_search(list, item):
    # low and high keep track of which part of the list you'll search in
    low = 0
    high = len(list) - 1

    while low <= high:
        # Check the middle element
        mid = (low + high) // 2
        guess = list[mid]

        # Found the item.
        if guess == item:
            return mid
        
        # The guess was too high.
        if guess > item:
            high = mid - 1

        # The guess was too low.
        else:
            low = mid + 1
    return None

if __name__ == '__main__':
    my_list = [1, 3, 5, 7, 9, 27, 29, 235, 4567]
    my_list.sort()
    print(binary_search(my_list, 27))