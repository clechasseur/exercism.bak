"
I can transform the way letters and scores are stored for an online multiplayer game.
"
Class {
	#name : #Etl,
	#superclass : #Object,
	#category : #'Exercise@Etl'
}

{ #category : #converting }
Etl class >> asLettersToScores: scoreToLetters [
	"Given an association between a score (as a string) and a list of uppercase letters (as strings), answers a list of associations between the lowercased letters and their scores (as numbers)."
	
	| score |
	score := scoreToLetters key asInteger.
	
	 ^ scoreToLetters value collect: [ :letter | (letter asLowercase) -> score ]
]

{ #category : #transforming }
Etl >> transformData: scoresToLetters [
	"Transforms the given letter and score data from the old to the new format.
	
	The old format is a dictionary where keys are the score and values are lists of uppercase letters with that score.
	
	The new format must be a dictionary where keys are the lowercase letters and values are their score."
	
	^ (scoresToLetters associations
		flatCollect: [ :scoreToLetters | self class asLettersToScores: scoreToLetters ])
		asDictionary
]
