package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
)

type Direction string

const (
	FILENAME           = "2025/01/input.txt"
	START              = 50
	LEFT     Direction = "L"
	RIGHT    Direction = "R"
)

func main() {
	file, err := os.Open(FILENAME)
	if err != nil {
		log.Panicf("error reading %v\n", FILENAME)
	}
	scanner := bufio.NewScanner(file)

	position := START
	timesAtZero := 0

	for scanner.Scan() {
		line := scanner.Text()
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

	log.Printf("Password: %d", timesAtZero)

	return
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
