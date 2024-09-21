"
I can help computer the resistance value of resistors from their color bands.
"
Class {
	#name : #ResistorColorDuo,
	#superclass : #Object,
	#classVars : [
		'Colors'
	],
	#category : #'Exercise@ResistorColorDuo'
}

{ #category : #'class initialization' }
ResistorColorDuo class >> initialize [
	"ResistorColorDuo initialize"
	
	Colors := #('black' 'brown' 'red' 'orange' 'yellow' 'green' 'blue' 'violet' 'grey' 'white' ).
]

{ #category : #accessing }
ResistorColorDuo >> colorValue: color [
	"Answers the value associated with the given color.
	Raises an error for unknown colors."
	
	"ResistorColorDuo new colorValue: 'brown' >>> 1"
	
	^ (Colors indexOf: color ifAbsent: [ self error: 'Invalid color' ]) - 1
]

{ #category : #accessing }
ResistorColorDuo >> valueWithColors: colors [ 
	"Given a list of two band colors, answers the resistance value.
	If there are more than two colors, the rest are ignored."
	
	"ResistorColorDuo new valueWithColors: #('yellow' 'violet' ) >>> 47"
	
	^ ((self colorValue: colors first) * 10) + (self colorValue: colors second)
]
