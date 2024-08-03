"
I can provide information about minutes.
"
Class {
	#name : #Minutes,
	#superclass : #Object,
	#category : #'Exercise@Clock'
}

{ #category : #testing }
Minutes class >> isAbstract [
	^ true
]

{ #category : #constants }
Minutes class >> perDay [
	"Answers the number of minutes in a day."
	
	^ (self perHour) * (Hours perDay)
]

{ #category : #constants }
Minutes class >> perHour [
	"Answers the number of minutes in an hour."
	
	^ 60
]
