def simple_search(list, item):
    for i in range(len(list)):
        if list[i] == item:
            return i
    return None

if __name__ == '__main__':
    my_list = [1, 3, 5, 7, 9, 27, 6, 235, 55]
    print(simple_search(my_list, 6))