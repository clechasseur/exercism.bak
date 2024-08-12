"
I represent a clock in 24-hour format.
"
Class {
	#name : #Clock,
	#superclass : #Object,
	#instVars : [
		'minutesInDay'
	],
	#category : #'Exercise@Clock'
}

{ #category : #initialization }
Clock class >> hour: hour minute: minute [
	"Answers a new Clock with the given hour and minute values."
	
	"(Clock hour: 1 minute: 40) asString >>> '01:40'"
	
	^ self new
		setHour: hour minute: minute;
		yourself
]

{ #category : #'instance creation' }
Clock class >> midnight [
	"Answers a clock representing midnight, e.g. 00:00."
	
	"Clock midnight asString >>> '00:00'"
	
	^ self hour: 0 minute: 0
]

{ #category : #initialization }
Clock class >> minutesInDay: minutes [
	"Answers a new Clock with the given number of minutes since start of day."
	
	"(Clock minutesInDay: 100) asString >>> '01:40'"
	
	^ self new
		setMinutesInDay: minutes;
		yourself
]

{ #category : #'instance creation' }
Clock class >> noon [
	"Answers a clock representing noon, e.g. 12:00."
	
	"Clock noon asString >>> '12:00'"
	
	^ self hour: 12 minute: 0
]

{ #category : #arithmetic }
Clock >> + minutes [
	"Answers a new clock, moved forward the given number of minutes."
	
	"((Clock hour: 1 minute: 40) + 40) asString >>> '02:20'"
	
	^ self class minutesInDay: minutesInDay + minutes
]

{ #category : #arithmetic }
Clock >> - minutes [
	"Answers a new clock, moved backwards the given number of minutes."
	
	"((Clock hour: 1 minute: 40) - 50) asString >>> '00:50'"
	
	^ self + (minutes negated)
]

{ #category : #comparing }
Clock >> < aClock [
	"Answers whether the receiver's time is before that of the given clock."
	
	"(Clock hour: 2 minute: 30) < (Clock hour: 14 minute: 10) >>> true"
	
	^ minutesInDay < (aClock minutesInDay)
]

{ #category : #comparing }
Clock >> <= aClock [
	"Answers whether the receiver's time is before or equal to that of the given clock."

	"(Clock hour: 2 minute: 30) <= (Clock hour: 14 minute: 10) >>> true"

	^ minutesInDay <= (aClock minutesInDay)
]

{ #category : #comparing }
Clock >> = aClock [
	"Answers whether the receiver has the same hour and minute values as the given clock."
	
	"(Clock hour: 1 minute: 40) = (Clock minutesInDay: 100) >>> true"
	
	^ minutesInDay = (aClock minutesInDay)
]

{ #category : #comparing }
Clock >> > aClock [
	"Answers whether the receiver's time is after that of the given clock."

	"(Clock hour: 18 minute: 30) > (Clock hour: 14 minute: 10) >>> true"

	^ minutesInDay > (aClock minutesInDay)
]

{ #category : #comparing }
Clock >> >= aClock [
	"Answers whether the receiver's time is after or equal to that of the given clock."

	"(Clock hour: 18 minute: 30) >= (Clock hour: 14 minute: 10) >>> true"

	^ minutesInDay >= (aClock minutesInDay)
]

{ #category : #comparing }
Clock >> hash [
	"Returns a hash value for the clock."
	
	^ minutesInDay hash
]

{ #category : #accessing }
Clock >> hour [
	"Answers the clock's hour value, in 24-hour format."
	
	^ minutesInDay // (Minutes perHour)
]

{ #category : #initialization }
Clock >> initialize [
	super initialize.
	
	minutesInDay := 0.
]

{ #category : #accessing }
Clock >> minute [
	"Answers the clock's minute value."
	
	^ minutesInDay \\ (Minutes perHour)
]

{ #category : #accessing }
Clock >> minutesInDay [
	"Answers the number of minutes elapsed since the start of the day."
	
	"(Clock hour: 1 minute: 40) minutesInDay >>> 100"
	
	^ minutesInDay
]

{ #category : #printing }
Clock >> printOn: aStream [
	"Prints a 24-hour representation of the receiver to the given stream."
	
	aStream << (self hour asTwoCharacterString) << ':' << (self minute asTwoCharacterString)
]

{ #category : #'private - initialization' }
Clock >> setHour: hour minute: minute [
	"Sets the clock's hour and minute values."
	
	"Clock new setHour: 1 minute: 20; asString >>> '01:20'"
	
	self setMinutesInDay: hour * (Minutes perHour) + minute.
]

{ #category : #'private - initialization' }
Clock >> setMinutesInDay: minutes [
	"Sets the clock's minutes per day. This value is used to determine the clock's hour and minute values."
	
	"Clock new setMinutesInDay: 100; asString >>> '01:40'"
	
	minutesInDay := minutes \\ (Minutes perDay).
]
