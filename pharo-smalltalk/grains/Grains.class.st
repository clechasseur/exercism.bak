"
I can calculate the number of grains on a square of a chessboard, when grains are placed on squares in an exponential manner (1, 2, 4, 8...).

```
Grains atSquare: 1 >>> 1
```

```
Grains atSquare: 64 >>> 9223372036854775808
```
"
Class {
	#name : #Grains,
	#superclass : #Object,
	#classVars : [
		'NumSquares'
	],
	#category : #'Exercise@Grains'
}

{ #category : #'class initialization' }
Grains class >> initialize [
	"Grains initialize"	
			
	NumSquares := 64.
]

{ #category : #accessing }
Grains class >> squares [
	"Returns an Interval with the numbers of every square on the chessboard."	
			
	^ 1 to: NumSquares
]

{ #category : #validations }
Grains class >> validSquare: square [
	"Checks if the given number represents a valid square on the chessboard."
	
	^ square between: 1 and: NumSquares
]

{ #category : #accessing }
Grains >> atSquare: square [
	"Returns the number of grains on the given square of the chessboard."
	
	(self class validSquare: square)
		ifFalse: [ self error: 'square must be between 1 and 64' ].
		
	^ square = 1
		ifTrue: [ 1 ]
		ifFalse: [ 2 * (self atSquare: square - 1) ]
]

{ #category : #accessing }
Grains >> total [
	"Returns the total number of grains on all squares of the chessboard."
	
	^ self class squares sum: [ :square | self atSquare: square ]
]
