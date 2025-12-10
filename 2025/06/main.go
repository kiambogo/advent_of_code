package main

import (
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

const (
	FILENAME = "2025/06/input.txt"
)

type operation struct {
	str string
	fn  func(x, y int) int
}

var (
	plus *operation = &operation{
		str: "+",
		fn:  func(x, y int) int { return x + y },
	}
	multiply *operation = &operation{
		str: "*",
		fn:  func(x, y int) int { return x * y },
	}
)

type equation struct {
	nums []int
	op   *operation
}

func (e equation) String() string {
	strs := []string{}
	for _, n := range e.nums {
		strs = append(strs, fmt.Sprint(n))
	}
	return strings.Join(strs, e.op.str)
}

func (e equation) calculate() int {
	if len(e.nums) == 0 {
		return 0
	}
	if len(e.nums) == 1 {
		return e.nums[0]
	}
	total := 0
	// we are using total as the starting value. for addition, we base off 0; multiplication base off 1
	if e.op.str == "*" {
		total = 1
	}
	for i := 0; i < len(e.nums); i += 1 {
		total = e.op.fn(total, e.nums[i])
	}
	return total
}

func main() {
	data, err := os.ReadFile(FILENAME)
	if err != nil {
		log.Panicf("error reading %v: %v\n", FILENAME, err.Error())
	}
	lines := strings.Split(string(data), "\n")
	equations := parseEquations(lines)

	pt1Answer := calculatePt1Solution(equations)

	log.Printf("PT1 Solution: %d", pt1Answer)

	return
}

func calculatePt1Solution(eqs map[int]*equation) int {
	total := 0
	for _, v := range eqs {
		d := v.calculate()
		total += d
	}
	return total
}

func parseEquations(lines []string) map[int]*equation {
	equations := make(map[int]*equation)
	for lineNum, line := range lines {
		if line == "" {
			continue
		}
		words := splitByWord(line)
		for wordNum, word := range words {
			eq, ok := equations[wordNum]
			if !ok {
				eq = &equation{}
			}
			if word == "*" {
				eq.op = multiply
				continue
			}
			if word == "+" {
				eq.op = plus
				continue
			}

			num, err := strconv.Atoi(word)
			if err != nil {
				log.Panicf("invalid word found on line %d: '%v'", lineNum, word)
			}
			eq.nums = append(eq.nums, num)
			equations[wordNum] = eq
		}
	}
	return equations
}

// splitByWord will go through a string and gather the space-delimited values from it
func splitByWord(line string) []string {
	words := []string{}
	word := strings.Builder{}
	for _, r := range line {
		if string(r) == " " {
			if word.Len() > 0 {
				words = append(words, word.String())
				word.Reset()
			}
			continue
		}
		_, err := word.WriteRune(r)
		if err != nil {
			log.Panicf("error writing rune to string builder: %v", err.Error())
		}
	}
	// after finishing the line, check if we have accumulated a word to flush
	if word.Len() > 0 {
		words = append(words, word.String())
		word.Reset()
	}
	return words
}
