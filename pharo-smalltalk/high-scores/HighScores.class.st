"
I handle top scores for a Frogger video game.
"
Class {
	#name : #HighScores,
	#superclass : #Object,
	#instVars : [
		'scores'
	],
	#category : #'Exercise@HighScores'
}

{ #category : #adding }
HighScores >> addScores: newScores [
	"Adds the given scores to the high scores list."
	
	scores addAll: newScores.
]

{ #category : #initialization }
HighScores >> initialize [
	super initialize.
	
	scores := OrderedCollection new.
]

{ #category : #accessing }
HighScores >> latestScore [
	"Answers the last score added to the high scores list."
	
	^ scores last
]

{ #category : #accessing }
HighScores >> personalBestScores [
	"Answers the highest score in the high scores list."
	
	^ scores max
]

{ #category : #accessing }
HighScores >> personalTopThreeScores [
	"Answers an array with the top three scores in the high scores list, in descending order."
	
	| sortedScores topX |
	
	sortedScores := scores sorted: [ :a :b | a > b ].
	topX := 3 min: sortedScores size.
	^ (sortedScores first: topX) asArray
]

{ #category : #accessing }
HighScores >> scores [
	"Answers an array with the high scores, in the order they were added."

	^ scores asArray
]
