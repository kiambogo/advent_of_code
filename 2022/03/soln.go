package main

import (
	"io/ioutil"
	"log"
	"strings"
)

type rucksack struct {
	compartment1 string
	compartment2 string
}

func priority(c rune) int {
	if int(c) >= 97 {
		return int(c) - 96
	}
	return int(c) - 38
}

func main() {
	bags := parseInput1("input.txt")
	totalPriority := 0
	for _, bag := range bags {
		sharedItems := map[rune]struct{}{}
		for _, c := range bag.compartment1 {
			for _, c2 := range bag.compartment2 {
				if c == c2 {
					sharedItems[c] = struct{}{}
				}
			}
		}
		for item, _ := range sharedItems {
			totalPriority += priority(item)
		}
	}
	log.Printf("Soln 1: Total score: %v\n", totalPriority)

	totalPriority = 0
	bagSets := parseInput2("input.txt")
	for _, set := range bagSets {
		common := commonSet(set...)
		for item, _ := range common {
			totalPriority += priority(item)
		}
	}
	log.Printf("Soln 2: Total score: %v\n", totalPriority)
}

func parseInput1(filename string) (bags []rucksack) {
	data, err := ioutil.ReadFile(filename)
	if err != nil {
		log.Fatalln(err)
	}
	lines := strings.Split(string(data), "\n")

	for _, line := range lines {
		if line == "" {
			break
		}
		bag := rucksack{
			compartment1: line[:len(line)/2],
			compartment2: line[len(line)/2:],
		}
		bags = append(bags, bag)
	}
	return
}

func parseInput2(filename string) (bagSets [][]rucksack) {
	data, err := ioutil.ReadFile(filename)
	if err != nil {
		log.Fatalln(err)
	}
	lines := strings.Split(string(data), "\n")

	finished := false
	pos := 0
	for finished == false {
		bagSet := []rucksack{}
		for i := 0; i < 3; i++ {
			line := lines[pos+i]
			if line == "" {
				finished = true
				break
			}
			bag := rucksack{
				compartment1: line[:len(line)/2],
				compartment2: line[len(line)/2:],
			}
			bagSet = append(bagSet, bag)
		}
		bagSets = append(bagSets, bagSet)
		pos += 3
	}
	return
}

func union(a map[rune]struct{}, b map[rune]struct{}) map[rune]struct{} {
	result := map[rune]struct{}{}
	for item, _ := range a {
		if _, ok := b[item]; ok {
			result[item] = struct{}{}
		}
	}
	return result
}

func commonSet(bags ...rucksack) map[rune]struct{} {
	if len(bags) == 0 {
		return map[rune]struct{}{}
	}
	items := map[rune]struct{}{}
	if len(bags) == 1 {
		bag := bags[0]
		for _, c := range bag.compartment1 + bag.compartment2 {
			items[c] = struct{}{}
		}
		return items
	}

	bag := bags[0]
	for _, c := range bag.compartment1 + bag.compartment2 {
		items[c] = struct{}{}
	}

	next := bags[1:]
	return union(items, commonSet(next...))
}
