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
	
	^ matrix lines collect: [ :row | row parseLiterals at: aColumn ]
]

{ #category : #accessing }
Matrix >> input: matrix atRow: aRow [ 
	"Returns the given row of the matrix."
	
	^ (matrix lines at: aRow) parseLiterals
]
