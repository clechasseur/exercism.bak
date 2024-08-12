"
I am Jack's representation of a minefield in a game of Minesweeper.
"
Class {
	#name : #Minefield,
	#superclass : #Object,
	#instVars : [
		'mines',
		'height',
		'width'
	],
	#category : #'Exercise@Minesweeper'
}

{ #category : #'instance creation' }
Minefield class >> fromList: aMinesList [
	"Given a list of minefield lines as Strings, returns a new Minefield."
	
	| mines height width |
	mines := Set withAll: (OrderedCollection streamContents: [ :minesStream |
									aMinesList withIndexDo: [ :line :y |
										line withIndexDo: [ :c :x |
											c = $* ifTrue: [ minesStream << (x@y) ] ] ] ]).
	height := aMinesList size.
	width := height = 0 ifFalse: [ aMinesList first size ] ifTrue: [ 0 ].
					
	^ self new setMines: mines andHeight: height andWidth: width
]

{ #category : #accessing }
Minefield >> annotationFor: aPoint [
	"Returns the annotation to display for a specific field point."
	
	"- If `aPoint` has a mine on it, return `$*`."
	"- Otherwise, if `aPoint` has mines around it, returns the number of surrounding mines (as a `Character`)."
	"- Otherwise, returns an empty space."
	
	| surroundingMines |
	(self mineAt: aPoint) ifTrue: [ ^ $* ].
	
	surroundingMines := self minesAround: aPoint.
	^ surroundingMines = 0
		ifTrue: [ Character space ]
		ifFalse: [ surroundingMines asCharacterDigit ]
]

{ #category : #converting }
Minefield >> asAnnotatedList [
	"Returns the minefield as a list of Strings, with spaces annotated with the number of surrounding mines."
	
	^ self rowIndices collect: [ :y |
		self columnIndices collect: [ :x | self annotationFor: (x@y) ] as: String ]
]

{ #category : #accessing }
Minefield >> columnIndices [
	"Returns an Interval of indices for minefield columns."
	
	"The returned interval will be empty if the minefield has no column."
	
	^ 1 to: width
]

{ #category : #accessing }
Minefield >> height [
	"Returns the height of the minefield (i.e. the number of rows)."
	
	^ height
]

{ #category : #accessing }
Minefield >> mineAt: aPoint [
	"Checks if there is a mine at the given point in the minefield."
	
	^ mines includes: aPoint
]

{ #category : #accessing }
Minefield >> minesAround: aPoint [
	"Returns the number of mines around aPoint."
	
	^ aPoint eightNeighbors count: [ :pt | self mineAt: pt ]
]

{ #category : #accessing }
Minefield >> rowIndices [
	"Returns an Interval of indices for minefield rows."
	
	"The returned interval will be empty if the minefield has no row."
	
	^ 1 to: height
]

{ #category : #'private - initialization' }
Minefield >> setMines: aMinesSet andHeight: aHeight andWidth: aWidth [

	mines := aMinesSet.
	height := aHeight.
	width := aWidth.
]

{ #category : #accessing }
Minefield >> width [
	"Returns the width of the minefield (i.e. the number of columns)."
	
	^ width
]
