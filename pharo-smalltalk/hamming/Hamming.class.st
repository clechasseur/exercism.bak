"
I can calculate the Hamming Distance between two DNA strands.
"
Class {
	#name : #Hamming,
	#superclass : #Object,
	#category : #'Exercise@Hamming'
}

{ #category : #arithmetic }
Hamming >> distanceStrand1: aDnaStrand strand2: anotherDnaStrand [ 
	"Calculates the Hamming Distance between the two given DNA strands. The Hamming Distance is the number of differences between corresponding nucleotides in the two strands.
	
	An exception will be raised if either strand is empty or if their length is not equal."
	
	"Hamming new distanceStrand1: 'ACGT' strand2: 'ACGT' >>> 0"
	
	"Hamming new distanceStrand1: 'ACGT' strand2: 'CGTA' >>> 4"
	
	| size1 size2 |
	size1 := aDnaStrand size.
	size2 := anotherDnaStrand size.
	
	size1 isZero
		ifTrue: [ size2 isZero ifFalse: [ self error: 'left strand must not be empty' ] ]
		ifFalse: [ size2 isZero ifTrue: [ self error: 'right strand must not be empty' ] ].
	(size1 == size2) ifFalse: [ self error: 'left and right strands must be of equal length' ].
	
	^ aDnaStrand zip: anotherDnaStrand thenCount: [ :n1 :n2 | (n1 == n2) not ]
]
