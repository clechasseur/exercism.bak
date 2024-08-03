"
I produce nice diamond shapes with letters in them. Example:

```
Diamond new rowsLetter: 'E' >>> #( '    A    '
                                   '   B B   '
                                   '  C   C  '
                                   ' D     D '
                                   'E       E'
                                   ' D     D '
                                   '  C   C  '
                                   '   B B   '
                                   '    A    ' )
```
"
Class {
	#name : #Diamond,
	#superclass : #Object,
	#category : #'Exercise@Diamond'
}

{ #category : #private }
Diamond class >> allNonLimitRowsForLast: lastCharacter [
	"Answers a collection with all non-limit rows (e.g. rows for all letters but $A) for a diamond with the given last character, in ascending order of characters."
	
	(lastCharacter == $A) ifTrue: [ ^ #() ].
	^ ($B to: lastCharacter) collect: [ :c | self rowFor: c forLast: lastCharacter ]
]

{ #category : #private }
Diamond class >> allRowsForLast: lastCharacter [
	"Answers a collection with all rows for a diamond with the given last character, in ascending order of characters."
	
	^ (Array with: (self limitRowForLast: lastCharacter)), (self allNonLimitRowsForLast: lastCharacter)
]

{ #category : #private }
Diamond class >> limitRowForLast: lastCharacter [
	"Answers the line containing the $A character for a diamond with the given last character."
	
	"Diamond limitRowForLast: $C >>> '  A  '"
	
	| whitespace |
	
	whitespace := ' ' repeat: (lastCharacter asciiValue) - ($A asciiValue).
	^ whitespace, 'A', whitespace
]

{ #category : #private }
Diamond class >> rowFor: character forLast: lastCharacter [
	"Answers the line containing the given character for a diamond with the given last character."
	
	"Diamond rowFor: $C forLast: $E >>> '  C   C  '"
	
	| outerWhitespace innerWhitespace charString |
	
	outerWhitespace := ' ' repeat: (lastCharacter asciiValue) - (character asciiValue).
	innerWhitespace :=  ' ' repeat: ((character asciiValue) - ($A asciiValue)) * 2 - 1.
	charString := character asString.
	^ outerWhitespace, charString, innerWhitespace, charString, outerWhitespace
]

{ #category : #drawing }
Diamond >> rowsLetter: lastLetter [
	"Answers an array of lines for a diamond up to the given last letter. See class comment for an example."
	
	| lastCharacter topHalf bottomHalf |
	
	lastCharacter := lastLetter first.
	
	topHalf := self class allRowsForLast: lastCharacter.
	bottomHalf := topHalf allButLast reversed.
	^ topHalf, bottomHalf
]
