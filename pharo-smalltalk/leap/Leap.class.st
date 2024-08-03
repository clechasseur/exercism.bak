Class {
	#name : #Leap,
	#superclass : #Object,
	#category : #'Exercise@Leap'
}

{ #category : #utilities }
Leap >> leapYear: aYear [
	"Checks if the given year is a leap year."
	"A year is a leap year if it is divisible by 4 AND not divisible by 100 UNLESS it's divisible by 400."
	| adjustedYear |
	
	"Adjust year, because year 0 does not exist and is therefore not considered a leap year."
	"Thus, year -1 (1 BCE) is a leap year, and so forth."
	adjustedYear := aYear > 0
		ifTrue: [ aYear ]
		ifFalse: [ (aYear + 1) negated ].
		
	^ (adjustedYear isDivisibleBy: 400) or: [ (adjustedYear isDivisibleBy: 4) and: [ (adjustedYear isDivisibleBy: 100) not ] ]
]
