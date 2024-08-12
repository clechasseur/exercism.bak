"
I can translate numbers into roman numerals.

```
RomanNumerals new romanNumber: 1990 >>> 'MCMXC'
```

```
RomanNumerals new romanNumber: 123 >>> 'MXXIII'
```
"
Class {
	#name : #RomanNumerals,
	#superclass : #Object,
	#classVars : [
		'RomanNumberGenerators',
		'RomanNumbers'
	],
	#category : #'Exercise@RomanNumerals'
}

{ #category : #'class initialization' }
RomanNumerals class >> initialize [
	"If you modify this method, select the line below and choose Do It:"
	"RomanNumerals initialize"

	RomanNumbers := {
		self numeralsWithOne: 'I' five: 'V' ten: 'X'.
		self numeralsWithOne: 'X' five: 'L' ten: 'C'.
		self numeralsWithOne: 'C' five: 'D' ten: 'M'.
		self numeralsWithOne: 'M' five: '!' ten: '!'
	}.
	
	RomanNumberGenerators := {
		#romanNumberBelowFour:withNumbers:.
		#romanNumberBelowFour:withNumbers:.
		#romanNumberBelowFour:withNumbers:.
		#romanNumberBelowFour:withNumbers:.
		#romanNumberFour:withNumbers:.
		#romanNumberBelowNine:withNumbers:.
		#romanNumberBelowNine:withNumbers:.
		#romanNumberBelowNine:withNumbers:.
		#romanNumberBelowNine:withNumbers:.
		#romanNumberNine:withNumbers:
	}.
]

{ #category : #private }
RomanNumerals class >> numberParts: number [
	| remain parts |
	remain := number.
	parts := OrderedCollection new.
	
	[ remain > 0 ]
		whileTrue: [
			parts add: remain \\ 10.
			remain := remain // 10.
		].
	
	^ parts
]

{ #category : #private }
RomanNumerals class >> numeralsWithOne: one five: five ten: ten [
	^ IdentityDictionary newFromKeys: { #one. #five. #ten } andValues: { one. five. ten }
]

{ #category : #private }
RomanNumerals class >> romanNumberBelowFour: part withNumbers: numbers [
	^ (numbers at: #one) repeat: part
]

{ #category : #private }
RomanNumerals class >> romanNumberBelowNine: part withNumbers: numbers [
	^ (numbers at: #five), (self romanNumberBelowFour: part - 5 withNumbers: numbers)
]

{ #category : #private }
RomanNumerals class >> romanNumberFour: part withNumbers: numbers [
	^ (numbers at: #one), (numbers at: #five)
]

{ #category : #private }
RomanNumerals class >> romanNumberNine: part withNumbers: numbers [
	^ (numbers at: #one), (numbers at: #ten)
]

{ #category : #private }
RomanNumerals class >> romanNumberPart: part withNumbers: numbers [
	^ self perform: (RomanNumberGenerators at: part + 1) with: part with: numbers
]

{ #category : #'roman numbers' }
RomanNumerals >> romanNumber: aNumber [ 
	"Returns a string containing the representation of aNumber into roman numerals."
	
	| parts romanParts |
	parts := self class numberParts: aNumber.
	romanParts := parts with: (RomanNumbers first: parts size) collect: [ :part :numbers |
		self class romanNumberPart: part withNumbers: numbers
	].

	^ '' join: romanParts reversed
]
