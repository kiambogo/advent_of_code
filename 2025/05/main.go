package main

import (
	"log"
	"os"
	"regexp"
	"slices"
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
	pt2Answer := calculatePt2Solution(ranges)

	log.Printf("PT1 Solution: %d", pt1Answer)
	log.Printf("PT2 Solution: %d", pt2Answer)

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

func calculatePt2Solution(ranges []numRange) int {
	total := 0

	cmp := func(a, b numRange) int {
		if a.lower < b.lower {
			return -1
		}
		if a.lower > b.lower {
			return 1
		}
		return 0
	}

	// sort the ranges by lower val, then merge them
	slices.SortFunc(ranges, cmp)

	mergedRanges := []numRange{}

	// keep a set of 'merged' ranges
	// for each range, go through all merged ranges and either merge with one fo those, or append to the merged rnages
	// at the very end, go through each and caluclate the total cumulative size of all merged ranges

	for i := range ranges {
		// there are two ranges we consider: the 'runner' and the 'sample'
		// the 'sample' is the range that we are currently investigating; the 'runner' is the current range of the merged ranges we are evaluating it against
		// so for each sample we will:
		// check if the 'sample' lower number is less than the upper of the 'runner'
		// if true, then the 'sample' fits within the 'runner' --> extend the runner

		mergedRanges = mergeOrAppend(mergedRanges, ranges[i])
	}
	for _, mr := range mergedRanges {
		total += (mr.upper - mr.lower) + 1 // add one due to the fact that upper-lower is zero based
	}

	return total
}

func mergeOrAppend(mergedRanges []numRange, entry numRange) []numRange {
	overlaps := func(rng, entry numRange) bool {
		if entry.lower <= rng.upper && entry.lower >= rng.lower {
			return true
		}
		if entry.upper <= rng.upper && entry.upper >= rng.lower {
			return true
		}
		return false
	}

	if len(mergedRanges) == 0 {
		return []numRange{entry}
	}

	for i, mr := range mergedRanges {
		if overlaps(mr, entry) {
			// either reset lower
			// or reset upper
			// or reset both if the range completely fits within the new entry
			if entry.lower < mr.lower {
				mergedRanges[i].lower = entry.lower
			}
			if entry.upper > mr.upper {
				mergedRanges[i].upper = entry.upper
			}
			return mergedRanges
		}
	}

	return append(mergedRanges, entry)
}
