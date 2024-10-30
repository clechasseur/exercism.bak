package foodchain

import (
	"fmt"
	"strings"
)

func Verse(v int) string {
	return Verses(v, v)
}

func Verses(start, end int) string {
	builder := &strings.Builder{}

	for v := start; v <= end; v++ {
		addVerse(v, builder)
		if v < end {
			builder.WriteString("\n\n")
		}
	}

	return builder.String()
}

func Song() string {
	return Verses(1, 8)
}

var animals = [...]string{"fly", "spider", "bird", "cat", "dog", "goat", "cow", "horse"}
var secondLines = [...]string{
	"It wriggled and jiggled and tickled inside her.",
	"How absurd to swallow a bird!",
	"Imagine that, to swallow a cat!",
	"What a hog, to swallow a dog!",
	"Just opened her throat and swallowed a goat!",
	"I don't know how she swallowed a cow!",
}

const firstLineFmt = "I know an old lady who swallowed a %s.\n"
const recursiveLineFmt = "She swallowed the %s to catch the %s%s.\n"
const spiderAddon = " that wriggled and jiggled and tickled inside her"
const lastLine = "I don't know why she swallowed the fly. Perhaps she'll die."
const lastLastLine = "She's dead, of course!"

func addFirstLine(v int, builder *strings.Builder) {
	builder.WriteString(fmt.Sprintf(firstLineFmt, animals[v-1]))
}

func addSecondLine(v int, builder *strings.Builder) {
	if v > 1 {
		builder.WriteString(secondLines[v-2])
		builder.WriteRune('\n')
	}
}

func addRecursiveLines(v int, builder *strings.Builder) {
	for r := v - 1; r > 0; r-- {
		addon := ""
		if r == 2 {
			addon = spiderAddon
		}

		builder.WriteString(fmt.Sprintf(recursiveLineFmt, animals[r], animals[r-1], addon))
	}
}

func addVerse(v int, builder *strings.Builder) {
	addFirstLine(v, builder)
	if v == 8 {
		builder.WriteString(lastLastLine)
	} else {
		addSecondLine(v, builder)
		addRecursiveLines(v, builder)
		builder.WriteString(lastLine)
	}
}
