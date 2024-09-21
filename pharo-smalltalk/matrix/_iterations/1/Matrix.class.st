"
I can extract the rows and columns from a matrix of numbers.
"
Class {
	#name : #Matrix,
	#superclass : #Object,
	#category : #'Exercise@Matrix'
}

{ #category : #accessing }
Matrix >> input: matrix atColumn: aColumn [ 
	"Returns the given column of the matrix."
	
	^ (matrix splitOn: Character cr)
		collect: [ :row | ((row splitOn: Character space) at: aColumn) asInteger ]
		as: Array
]

{ #category : #accessing }
Matrix >> input: matrix atRow: aRow [ 
	"Returns the given row of the matrix."
	
	| row |
	row := (matrix splitOn: Character cr) at: aRow.
	^ (row splitOn: Character space)
		collect: [ :n | n asInteger ]
		as: Array
]
