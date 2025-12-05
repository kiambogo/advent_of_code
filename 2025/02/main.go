package main

import (
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

const (
	FILENAME = "2025/02/input.txt"
)

func main() {
	data, err := os.ReadFile(FILENAME)
	if err != nil {
		log.Panicf("error reading %v\n", FILENAME)
	}
	lines := strings.Split(string(data), "\n")
	entries := strings.Split(lines[0], ",")

	pt1Answer := calculatePt1Solution(entries)
	pt2Answer := calculatePt2Solution(entries)

	log.Printf("PT1 Solution: %d", pt1Answer)
	log.Printf("PT2 Solution: %d", pt2Answer)

	return
}

func calculatePt1Solution(entries []string) int64 {
	total := int64(0)
	for _, entry := range entries {
		a, b, err := parseRange(entry)
		if err != nil {
			log.Panicln(err.Error())
		}
		for n := a; n <= b; n++ {
			if isRepeatedDigits(strconv.Itoa(n)) {
				total += int64(n)
			}
		}
	}
	return total
}

func calculatePt2Solution(entries []string) int64 {
	total := int64(0)
	for _, entry := range entries {
		a, b, err := parseRange(entry)
		if err != nil {
			log.Panicln(err.Error())
		}
		for n := a; n <= b; n++ {
			if hasNRepeatedDigits(strconv.Itoa(n)) {
				total += int64(n)
			}
		}
	}
	return total
}

func parseRange(str string) (a int, b int, err error) {
	parts := strings.Split(str, "-")
	if len(parts) != 2 {
		return 0, 0, fmt.Errorf("invalid range: %v", str)
	}
	a, err = strconv.Atoi(parts[0])
	if err != nil {
		return 0, 0, fmt.Errorf("invalid lower range: %v", parts[0])
	}
	b, err = strconv.Atoi(parts[1])
	if err != nil {
		return 0, 0, fmt.Errorf("invalid upper range: %v", parts[1])
	}
	return
}

func isRepeatedDigits(val string) bool {
	if len(val) < 2 {
		return false
	}

	return val[0:(len(val)/2)] == val[(len(val)/2):]
}

func hasNRepeatedDigits(val string) bool {
	for windowSize := 1; windowSize <= (len(val) / 2); windowSize++ {
		sample := val[0:windowSize]
		if stringIsSequenceOfRepeated(val, sample) {
			return true
		}
	}

	return false
}

func stringIsSequenceOfRepeated(str string, rep string) bool {
	if str == "" {
		return false
	}
	if len(str) < len(rep) {
		return false
	}
	if len(str) == len(rep) {
		return str == rep
	}
	return string(str[0:len(rep)]) == rep && stringIsSequenceOfRepeated(str[len(rep):], rep)
}
