Extension { #name : #Character }

{ #category : #'*Exercise@IsbnVerifier' }
Character >> isIsbn10 [
	"Answers whether the receiver is a valid ISBN-10 character.
	ISBN-10 accepts only digits and $X."
	
	^ self isDigit or: [ self == $X ]
]

{ #category : #'*Exercise@IsbnVerifier' }
Character >> asIsbn10Value [
	"Answers the receiver's value as an ISBN-10 character.
	In an ISBN-10 number, X has value 10."
	
	^ self == $X ifTrue: [ 10 ] ifFalse: [ self digitValue ]
]
