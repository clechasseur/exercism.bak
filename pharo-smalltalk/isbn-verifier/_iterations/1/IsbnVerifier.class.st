"
I can verify the validity of ISBN-10 book identification numbers.
"
Class {
	#name : #IsbnVerifier,
	#superclass : #Object,
	#category : #'Exercise@IsbnVerifier'
}

{ #category : #'private - computation' }
IsbnVerifier class >> checksumOf: isbn [
	"Answers the checksum of an ISBN-10 number.
	Answers nil if the ISBN-10 has invalid characters."
	
	^ (isbn inject: (10 -> 0) into: [ :mulSum :c |
		(self digitValue: c allowingX: (mulSum key == 1))
			ifNil: [ ^ nil ]
			ifNotNil: [ :value | (mulSum key - 1) -> (value * mulSum key + mulSum value) ] ]) value \\ 11
]

{ #category : #'private - conversion' }
IsbnVerifier class >> cleanIsbn: isbn [
	"Cleans the given ISBN-10 by removing any dash."
	
	^ isbn reject: [ :c | c == $- ]
]

{ #category : #'private - conversion' }
IsbnVerifier class >> digitValue: isbnDigit allowingX: xAllowed [
	"Answers the numeric value of an ISBN-10 digit.
	Answers nil if the digit is invalid (or not allowed)."
	
	isbnDigit isDigit ifTrue: [ ^ isbnDigit digitValue ].
	xAllowed ifFalse: [ ^ nil ].
	isbnDigit == $X ifTrue: [ ^ 10 ].
	^ nil
]

{ #category : #testing }
IsbnVerifier >> isValidIsbn: isbn [
	"Answers whether the given String is a valid ISBN-10 number.
	Dashes in the string are ignored."
	| cleanedIsbn |
	
	cleanedIsbn := self class cleanIsbn: isbn.
	^ cleanedIsbn size == 10 and: [ (self class checksumOf: cleanedIsbn) == 0 ]
]
