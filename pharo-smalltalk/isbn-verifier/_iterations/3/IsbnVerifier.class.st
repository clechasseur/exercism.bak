"
I can verify the validity of ISBN-10 book identification numbers.
"
Class {
	#name : #IsbnVerifier,
	#superclass : #Object,
	#category : #'Exercise@IsbnVerifier'
}

{ #category : #testing }
IsbnVerifier >> isValidIsbn: isbn [
	"Answers whether the given String is a valid ISBN-10 number.
	Dashes in the string are ignored."
	
	^ isbn isCharacter not and: [ isbn isIsbn10 ]
]
