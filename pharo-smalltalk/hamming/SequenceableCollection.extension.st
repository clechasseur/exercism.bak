Extension { #name : #SequenceableCollection }

{ #category : #'*Exercise@Hamming' }
SequenceableCollection >> zip: aCollection thenCount: aBlock [
	"Calls the given block with two corresponding elements of the receiver and the given collection, then counts the number of pair of elements for which the block returns true."
	
	| count |
	
	count := 0.
	self withIndexDo: [ :a :i |
		(aBlock value: a value: (aCollection at: i)) ifTrue: [ count := count + 1 ] ].
	^ count
]
