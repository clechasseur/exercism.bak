Extension { #name : #SequenceableCollection }

{ #category : #'*Exercise@IsbnVerifier' }
SequenceableCollection >> isbn10Checksum [
	"Answers the checksum of the receiver as an ISBN-10 number.
	Answers nil if the receiver contains invalid ISBN-10 characters or if the receiver does not have the correct number of ISBN-10 characters."
	| cleaned |
	
	cleaned := self asCleanedIsbn.
	(cleaned size == 10) ifFalse: [ ^ nil ].
	
	^ (cleaned inject: (10 -> 0) into: [ :mulSum :c |
		c isIsbn10 ifFalse: [ ^ nil ].
		(c == $X and: [ mulSum key > 1 ]) ifTrue: [ ^ nil ].
		(mulSum key - 1) -> (c asIsbn10Value * mulSum key + mulSum value) ]) value
]

{ #category : #'*Exercise@IsbnVerifier' }
SequenceableCollection >> asCleanedIsbn [
	"Cleans the receiver of dashes to make it a valid ISBN."
	
	^ self reject: [ :c | c == $- ]
]

{ #category : #'*Exercise@IsbnVerifier' }
SequenceableCollection >> isIsbn10 [
	"Answers whether the receiver is a valid ISBN-10 number."
	
	^ self isbn10Checksum
		ifNotNil: [ :checksum | checksum isDivisibleBy: 11 ]
		ifNil: [ false ]
]
