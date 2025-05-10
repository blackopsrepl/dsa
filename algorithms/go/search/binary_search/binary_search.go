package main

func binary_search(arr []int, target int) int {
	low := 0
	high := len(arr) - 1

	for low <= high {
		// Check the middle element
		mid := (low + high) / 2
		guess := arr[mid]

		// Found the target
		if guess == target {
			return mid
		}

		// The guess was too high
		if guess > target {
			high = mid - 1
			// The guess was too low
		} else {
			low = mid + 1
		}
	}
	return -1
}

func main() {
	arr := []int{1, 3, 5, 7, 9, 27, 29, 235, 4567}
	result := binary_search(arr, 27)
	println(result)
}
