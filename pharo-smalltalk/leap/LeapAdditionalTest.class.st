Class {
	#name : #LeapAdditionalTest,
	#superclass : #ExercismTest,
	#instVars : [
		'leapCalculator'
	],
	#category : #'Exercise@Leap'
}

{ #category : #config }
LeapAdditionalTest class >> includeInSolution [
	^ true
]

{ #category : #initialization }
LeapAdditionalTest >> setUp [
	super setUp.
	leapCalculator := Leap new.
]

{ #category : #tests }
LeapAdditionalTest >> testYear0DoesNotExistAndIsThereforeNotALeapYear [
	| result |
	
	result := leapCalculator leapYear: 0.
	self assert: result equals: false.
]

{ #category : #tests }
LeapAdditionalTest >> testYearMinus1LeapYear [
	| result |
	
	result := leapCalculator leapYear: -1.
	self assert: result equals: true.
]
