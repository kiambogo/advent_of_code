package main

import (
	"log"
	"os"
	"strings"
)

type Item string

const (
	FILENAME      = "2025/04/input.txt"
	Paper    Item = "@"
	Empty    Item = "."
)

func main() {
	data, err := os.ReadFile(FILENAME)
	if err != nil {
		log.Panicf("error reading %v\n", FILENAME)
	}
	lines := strings.Split(string(data), "\n")

	grid := buildGrid(lines)

	pt1Answer := calculatePt1Solution(grid)
	pt2Answer := calculatePt2Solution(grid)

	log.Printf("PT1 Solution: %d", pt1Answer)
	log.Printf("PT2 Solution: %d", pt2Answer)

	return

}
func buildGrid(lines []string) map[int]map[int]Item {
	grid := make(map[int]map[int]Item)
	for y, line := range lines {
		for x, item := range line {
			safeMapGet(grid, y)[x] = Item(item)
		}
	}
	return grid
}

func calculatePt1Solution(grid map[int]map[int]Item) int {
	total := 0
	for y, rowMap := range grid {
		for x, item := range rowMap {
			if item != Paper {
				continue
			}
			adjacentPaperCount := 0
		ROLL:
			for a := -1; a < 2; a++ {
				for b := -1; b < 2; b++ {
					if a == 0 && b == 0 {
						continue
					}
					if safeGridLookup(grid, x+a, y+b) == Paper {
						adjacentPaperCount++
					}
					if adjacentPaperCount > 4 {
						break ROLL
					}
				}
			}
			if adjacentPaperCount < 4 {
				total += 1
			}
		}
	}
	return total
}

func calculatePt2Solution(grid map[int]map[int]Item) int {
	removeFromGrid := func(g map[int]map[int]Item) (map[int]map[int]Item, int) {
		total := 0
		for y, rowMap := range g {
			for x, item := range rowMap {
				if item != Paper {
					continue
				}
				adjacentPaperCount := 0
			ROLL:
				for a := -1; a < 2; a++ {
					for b := -1; b < 2; b++ {
						if a == 0 && b == 0 {
							continue
						}
						if safeGridLookup(g, x+a, y+b) == Paper {
							adjacentPaperCount++
						}
						if adjacentPaperCount > 4 {
							break ROLL
						}
					}
				}
				if adjacentPaperCount < 4 {
					g[y][x] = Empty
					total += 1
				}
			}
		}

		return g, total
	}

	totalRemoved := 0
	newGrid, numRemoved := removeFromGrid(grid)
	for numRemoved > 0 {
		totalRemoved += numRemoved
		newGrid, numRemoved = removeFromGrid(newGrid)
	}

	return totalRemoved
}

// safeGridLookup will lookup an item at a specified location on the grid.
// if the space doesn't exist, consider it 'empty'
func safeGridLookup(grid map[int]map[int]Item, x, y int) Item {
	rowMap, ok := grid[y]
	if !ok {
		return Empty
	}
	item, ok := rowMap[x]
	if !ok {
		return Empty
	}
	return item
}

func safeMapGet(m map[int]map[int]Item, k int) map[int]Item {
	_, ok := m[k]
	if !ok {
		m[k] = make(map[int]Item)
	}
	return m[k]
}
