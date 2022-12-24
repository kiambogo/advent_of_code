package main

import (
	"io/ioutil"
	"log"
	"strings"
)

var (
	ROCK            int            = 1
	PAPER           int            = 2
	SCISSORS        int            = 3
	WIN             int            = 6
	DRAW            int            = 3
	LOSS            int            = 0
	nameToSelection map[string]int = map[string]int{
		"A": ROCK,
		"B": PAPER,
		"C": SCISSORS,
		// "X": ROCK,
		// "Y": PAPER,
		// "Z": SCISSORS,
		"X": LOSS,
		"Y": DRAW,
		"Z": WIN,
	}
)

type game struct {
	opponentSelection int
	mySelection       int
}

func (g game) score() int {
	switch g.opponentSelection {
	case ROCK:
		if g.mySelection == LOSS {
			return LOSS + SCISSORS
		}
		if g.mySelection == DRAW {
			return DRAW + ROCK
		}
		if g.mySelection == WIN {
			return WIN + PAPER
		}
	case PAPER:
		if g.mySelection == LOSS {
			return LOSS + ROCK
		}
		if g.mySelection == DRAW {
			return DRAW + PAPER
		}
		if g.mySelection == WIN {
			return WIN + SCISSORS
		}
	case SCISSORS:
		if g.mySelection == WIN {
			return WIN + ROCK
		}
		if g.mySelection == LOSS {
			return LOSS + PAPER
		}
		if g.mySelection == DRAW {
			return DRAW + SCISSORS
		}
	default:
		log.Fatalf("invalid selection")
	}
	return 0
}

func main() {
	games := parseInput("input.txt")
	totalScore := 0
	for _, game := range games {
		totalScore += game.score()
	}
	log.Printf("Total score: %v\n", totalScore)

}

func parseInput(filename string) (games []game) {
	data, err := ioutil.ReadFile(filename)
	if err != nil {
		log.Fatalln(err)
	}
	lines := strings.Split(string(data), "\n")

	for _, line := range lines {
		if line == "" {
			break
		}
		cols := strings.Split(line, " ")
		game := game{
			opponentSelection: nameToSelection[cols[0]],
			mySelection:       nameToSelection[cols[1]],
		}
		games = append(games, game)
	}
	return
}
