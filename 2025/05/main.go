package main

import (
	"log"
	"os"
	"regexp"
	"strconv"
	"strings"
)

const (
	FILENAME   = "2025/05/input.txt"
	rangeRegex = `^(?P<LOWER>\d*)-(?P<UPPER>\d*)$`
	entryRegex = `^(?P<ENTRY>\d*)$`
)

type numRange struct {
	lower int
	upper int
}

func NumRange(lower, upper int) numRange {
	return numRange{
		lower: lower,
		upper: upper,
	}
}

func main() {
	rangeRe := regexp.MustCompile(rangeRegex)
	entryRe := regexp.MustCompile(entryRegex)

	data, err := os.ReadFile(FILENAME)
	if err != nil {
		log.Panicf("error reading %v: %v\n", FILENAME, err.Error())
	}
	lines := strings.Split(string(data), "\n")

	ranges := []numRange{}
	nums := []int{}

	for lineIdx, line := range lines {
		if line == "" {
			continue
		}
		if rangeRe.MatchString(line) {
			lowerIdx := rangeRe.SubexpIndex("LOWER")
			upperIdx := rangeRe.SubexpIndex("UPPER")
			matches := rangeRe.FindStringSubmatch(line)
			lowerStr := matches[lowerIdx]
			upperStr := matches[upperIdx]
			lower, err := strconv.Atoi(lowerStr)
			if err != nil {
				log.Panicf("invalid lower number in range on line %d", lineIdx)
			}
			upper, err := strconv.Atoi(upperStr)
			if err != nil {
				log.Panicf("invalid upper number in range on line %d", lineIdx)
			}
			ranges = append(ranges, NumRange(lower, upper))
			continue
		}
		if entryRe.MatchString(line) {
			num, err := strconv.Atoi(line)
			if err != nil {
				log.Panicf("invalid number in list on line %d", lineIdx)
			}
			nums = append(nums, num)
		}
	}

	pt1Answer := calculatePt1Solution(ranges, nums)

	log.Printf("PT1 Solution: %d", pt1Answer)

	return
}

func calculatePt1Solution(ranges []numRange, nums []int) int {
	total := 0
NUMS:
	for _, num := range nums {
		for _, rng := range ranges {
			if num <= rng.upper && num >= rng.lower {
				total++
				continue NUMS
			}
		}
	}

	return total
}
