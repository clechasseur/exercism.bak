"
I play darts.
"
Class {
	#name : #Darts,
	#superclass : #Object,
	#classVars : [
		'ScoresByRadius'
	],
	#category : #'Exercise@Darts'
}

{ #category : #'class initialization' }
Darts class >> initialize [
	"Darts initialize"
	
	ScoresByRadius := {
		1.0  -> 10 .
		5.0  -> 5  .
		10.0 -> 1
	}.
]

{ #category : #scoring }
Darts >> scoreX: x y: y [ 
	"Answers the score of a dart throw landing at (x@y)."
	| radius |
	
	radius := (x@y) r.
	^ ScoresByRadius
		detect: [ :rToScore | radius <= (rToScore key) ]
		ifFound: [ :rToScore | rToScore value ]
		ifNone: [ 0 ]
]
