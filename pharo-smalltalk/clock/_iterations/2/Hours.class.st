"
I can provide information about hours.
"
Class {
	#name : #Hours,
	#superclass : #Object,
	#category : #'Exercise@Clock'
}

{ #category : #testing }
Hours class >> isAbstract [
	^ true
]

{ #category : #constants }
Hours class >> perDay [
	"Answers the number of hours in a day."
	
	^ 24
]
