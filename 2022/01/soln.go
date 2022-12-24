package main

import (
	"io/ioutil"
	"log"
	"sort"
	"strconv"
	"strings"
)

func main() {
	calsPerElf := parseInput("input.txt")

	sort.Slice(calsPerElf, func(i, j int) bool { return calsPerElf[i] > calsPerElf[j] })

	log.Printf("Max cals: %v\n", calsPerElf[0])
	log.Printf("Top 3 cals summed: %v\n", calsPerElf[0]+calsPerElf[1]+calsPerElf[2])
}

func parseInput(filename string) (calsPerElf []int) {
	data, err := ioutil.ReadFile(filename)
	if err != nil {
		log.Fatalln(err)
	}
	lines := strings.Split(string(data), "\n")

	total := 0
	for _, line := range lines {
		if line == "" {
			calsPerElf = append(calsPerElf, total)
			total = 0
			continue
		}
		cals, err := strconv.Atoi(line)
		if err != nil {
			log.Fatalln(err)
		}
		total += cals
	}
	return
}
