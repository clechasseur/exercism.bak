"
I can flatten arrays (just like #flattened, but 42% zazzier).
"
Class {
	#name : #FlattenArray,
	#superclass : #Object,
	#category : #'Exercise@FlattenArray'
}

{ #category : #enumerating }
FlattenArray class >> flattenNotNil: sourceCollection in: destCollection [
	"Flattens all levels of the given source collection and stores the non-nil elements in the given destination collection.
	Answers the destination collection."
	
	sourceCollection do: [ :each |
		each ifNotNil: [ 
			each isCollection
				ifTrue: [ self flattenNotNil: each in: destCollection ]
				ifFalse: [ destCollection add: each ] ] ].
		
	^ destCollection
]

{ #category : #enumerating }
FlattenArray >> flattenArray: aCollection [
	"Answers a new collection containing all non-nil elements of the given collection with all levels of sub-collections flattened."
	
	"Note: I'm purposefully not using #flattened here to try not to 'cheat'."
	^ (self class flattenNotNil: aCollection in: OrderedCollection new) as: aCollection species
]
