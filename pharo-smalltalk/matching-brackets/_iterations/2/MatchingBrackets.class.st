"
I can determine if brackets, parentheses and curly braces are correctly paired in a string.
"
Class {
	#name : #MatchingBrackets,
	#superclass : #Object,
	#classVars : [
		'ClosingPunctuations'
	],
	#category : #'Exercise@MatchingBrackets'
}

{ #category : #'class initialization' }
MatchingBrackets class >> initialize [
	"MatchingBrackets initialize"
	
	ClosingPunctuations := {
		$[ -> $] .
		$( -> $) .
		${ -> $}
	} as: IdentityDictionary.
]

{ #category : #private }
MatchingBrackets class >> isCharacter: aCharacter onTopOf: stack [
	"Pops the top of the given stack and answers whether it's the given character."
	
	^ stack isNotEmpty and: [ stack pop == aCharacter ]
]

{ #category : #testing }
MatchingBrackets >> isPaired: aString [ 
	"Answers whether the square brackets (`[]`), parentheses (`()`) and curly braces (`{}`) are correctly matched in the given string."
	
	| stack |
	stack := Stack new.
	
	aString do: [ :c |
		c isOpenPunctuation ifTrue: [ stack push: (ClosingPunctuations at: c) ].
		c isClosePunctuation ifTrue: [
			(self class isCharacter: c onTopOf: stack) ifFalse: [ ^ false ] ] ].
		
	^ stack isEmpty
]
