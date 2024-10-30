package pangram

import (
	"strings"
	"unicode"
)

const allLetters = (1 << 26) - 1

func IsPangram(input string) bool {
	letters := 0

	for _, c := range strings.ToLower(input) {
		if unicode.IsLetter(c) {
			letters |= 1 << int(c-'a')
			if letters == allLetters {
				return true
			}
		}
	}

	return false
}
