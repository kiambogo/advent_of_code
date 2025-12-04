package main

import (
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

type Direction string

const (
	FILENAME           = "2025/01/input.txt"
	START              = 50
	LEFT     Direction = "L"
	RIGHT    Direction = "R"
)

func main() {
	data, err := os.ReadFile(FILENAME)
	if err != nil {
		log.Panicf("error reading %v\n", FILENAME)
	}
	lines := strings.Split(string(data), "\n")

	pt1Answer := calculatePasswordPt1(lines)
	pt2Answer := calculatePasswordPt2(lines)

	log.Printf("PT1 Password: %d", pt1Answer)
	log.Printf("PT2 Password: %d", pt2Answer)

	return
}

func calculatePasswordPt1(lines []string) int {
	position := START
	timesAtZero := 0
	for _, line := range lines {
		if line == "" {
			continue
		}
		direction, count, err := parseLine(line)
		if err != nil {
			log.Panicf("invalid line found! %v\n", err)
		}
		switch direction {
		case LEFT:
			position -= count
		case RIGHT:
			position += count
		default:
			log.Panicf("invalid direction found! %v\n", direction)
		}
		position = mod(position, 100)
		if position == 0 {
			timesAtZero++
		}
	}
	return timesAtZero
}

func calculatePasswordPt2(lines []string) int {
	position := START
	timesAtZero := 0
	timesPassedZero := 0

	for _, line := range lines {
		if line == "" {
			continue
		}
		direction, count, err := parseLine(line)
		if err != nil {
			log.Panicf("invalid line found! %v\n", err)
		}

		thisTimesPassedZero := 0
		// 1. increment counter for each time we hit 0 while we rotate
		switch direction {
		case LEFT:
			endPosition := position - count
			if abs(endPosition) > 100 {
				thisTimesPassedZero += (abs(endPosition) / 100)
				timesPassedZero += (abs(endPosition) / 100)
				// if we end on 0 we'll count the final stop as a 'rotation'; subtract 1 to allow for the addition later
				if mod(endPosition, 100) == 0 {
					timesPassedZero--
				}
			}
			if signDiff(position, endPosition) {
				thisTimesPassedZero++
				timesPassedZero++
			}
			position = endPosition
		case RIGHT:
			endPosition := position + count
			if abs(endPosition) > 100 {
				thisTimesPassedZero += (abs(endPosition) / 100)
				timesPassedZero += (abs(endPosition) / 100)
				// if we end on 0 we'll count the final stop as a 'rotation'; subtract 1 to allow for the addition later
				if mod(endPosition, 100) == 0 {
					timesPassedZero--
				}
			}
			if signDiff(position, endPosition) {
				thisTimesPassedZero++
				timesPassedZero++
			}
			position = endPosition
		default:
			log.Panicf("invalid direction found! %v\n", direction)
		}
		position = mod(position, 100)

		// 2. increment counter if we stopped at 0
		if position == 0 {
			timesAtZero++
		}
	}

	return timesAtZero + timesPassedZero
}

func parseLine(line string) (direction Direction, count int, err error) {
	if line == "" {
		return "", 0, fmt.Errorf("empty line")
	}

	if len(line) < 2 {
		return "", 0, fmt.Errorf("len(line) < 2")
	}

	// Check first char is either L or R
	direction = Direction(line[0])
	if direction != LEFT && direction != RIGHT {
		return "", 0, fmt.Errorf("expected first char to be one of [%v, %v]; got %v", LEFT, RIGHT, line[0])
	}

	// Check rest of string is a number
	count, err = strconv.Atoi(line[1:])
	if err != nil {
		return "", 0, err
	}

	return
}

func mod(a, b int) int {
	return ((a % b) + b) % b
}

func signDiff(a, b int) bool {
	if a < 0 && b > 0 {
		return true
	}
	if a > 0 && b < 0 {
		return true
	}
	return false
}

func abs(a int) int {
	if a < 0 {
		return a * -1
	}
	return a
}
