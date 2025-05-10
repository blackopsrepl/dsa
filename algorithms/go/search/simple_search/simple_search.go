package main

func simple_search(arr []int, target int) int {
	for i := 0; i < len(arr); i++ {
		if arr[i] == target {
			return i
		}
	}
	return -1
}

func main() {
	arr := []int{1, 3, 5, 6, 9, 27, 7, 235, 55}
	result := simple_search(arr, 6)
	println(result)
}
