package main

import (
	"log"
	"os"
	"strconv"
	"strings"
)

const (
	FILENAME = "2025/03/input.txt"
)

func main() {
	data, err := os.ReadFile(FILENAME)
	if err != nil {
		log.Panicf("error reading %v\n", FILENAME)
	}
	lines := strings.Split(string(data), "\n")

	entries := [][]int{}
	for lineNum, line := range lines {
		nums := []int{}
		for _, c := range line {
			n, err := strconv.Atoi(string(c))
			if err != nil {
				log.Panicf("invalid number '%v' on line %d", c, lineNum)
			}
			nums = append(nums, n)
		}
		entries = append(entries, nums)
	}

	pt1Answer := calculatePt1Solution(entries)
	pt2Answer := calculatePt2Solution(entries)

	log.Printf("PT1 Solution: %d", pt1Answer)
	log.Printf("PT2 Solution: %d", pt2Answer)

	return

}

func calculatePt1Solution(banks [][]int) int {
	total := 0
	for _, bank := range banks {
		if len(bank) == 0 {
			continue
		}
		a, aIdx := largestNum(bank[0:len(bank)-1], 0)
		b, _ := largestNum(bank[aIdx+1:], 0)
		joltage, err := strconv.Atoi(strconv.Itoa(a) + strconv.Itoa(b))
		if err != nil {
			log.Panicf("invalid joltage calculated with %v + %v: %v", a, b, err.Error())
		}
		total += joltage
	}

	return total
}

func calculatePt2Solution(banks [][]int) int {
	total := 0
	for _, bank := range banks {
		if len(bank) == 0 {
			continue
		}
		if len(bank) < 12 {
			return 0
		}
		// find the largest num that still leaves 11 digits remaining
		numStr := ""
		idx := 0
		for i := 1; i <= 12; i++ {
			offset := 12 - i
			currMax, newIdx := largestNum(bank[idx:len(bank)-offset], idx)
			numStr += strconv.Itoa(currMax)
			idx = newIdx + 1
		}

		joltage, err := strconv.Atoi(numStr)
		if err != nil {
			log.Panicf("invalid joltage calculated with %v: %v", numStr, err.Error())
		}
		total += joltage
	}

	return total
}

func largestNum(nums []int, idx int) (n int, index int) {
	if len(nums) == 0 {
		return 0, 0
	}
	if len(nums) <= 1 {
		return nums[0], idx
	}
	n2, idx2 := largestNum(nums[1:], idx+1)
	if n2 > nums[0] {
		return n2, idx2
	}
	return nums[0], idx
}
