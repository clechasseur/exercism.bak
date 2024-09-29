package letter

// FreqMap records the frequency of each rune in a given text.
type FreqMap map[rune]int

// Frequency counts the frequency of each rune in a given text and returns this
// data as a FreqMap.
func Frequency(text string) FreqMap {
	frequencies := FreqMap{}
	for _, r := range text {
		frequencies[r]++
	}
	return frequencies
}

// ConcurrentFrequency counts the frequency of each rune in the given strings,
// by making use of concurrency.
func ConcurrentFrequency(texts []string) FreqMap {
	c := make(chan FreqMap, len(texts))
	for _, text := range texts {
		go func(text string) { c <- Frequency(text) }(text)
	}

	frequencies := FreqMap{}
	for range texts {
		for r, count := range <-c {
			frequencies[r] += count
		}
	}

	return frequencies
}
