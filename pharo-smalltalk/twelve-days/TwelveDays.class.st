"
I can recite the lyrics of the classical song, 'The Twelve Days of Christmas'.
"
Class {
	#name : #TwelveDays,
	#superclass : #Object,
	#classVars : [
		'Days',
		'Gifts'
	],
	#category : #'Exercise@TwelveDays'
}

{ #category : #accessing }
TwelveDays class >> giftsForDay: day [
	"Answers a list of all gifts for the given day, in the order the song requires them."
	
	^ Gifts last: day
]

{ #category : #'class initialization' }
TwelveDays class >> initialize [
	"TwelveDays initialize"
	
	Days := #('first' 'second' 'third' 'fourth' 'fifth' 'sixth' 'seventh' 'eighth' 'ninth' 'tenth' 'eleventh' 'twelfth' ).
	Gifts := #('twelve Drummers Drumming' 'eleven Pipers Piping' 'ten Lords-a-Leaping' 'nine Ladies Dancing' 'eight Maids-a-Milking' 'seven Swans-a-Swimming' 'six Geese-a-Laying' 'five Gold Rings' 'four Calling Birds' 'three French Hens' 'two Turtle Doves' 'a Partridge in a Pear Tree' ).
]

{ #category : #accessing }
TwelveDays class >> labelForDay: day [
	"Answers the label for the given day in the song."
	
	^ Days at: day
]

{ #category : #accessing }
TwelveDays class >> verseForDay: day [
	"Answers the song's verse for the given day."
	
	^ String streamContents: [ :verseStream |
		| gifts |
		gifts := self giftsForDay: day.
		
		verseStream << 'On the '
						<< (self labelForDay: day)
						<< ' day of Christmas my true love gave to me: '.
		gifts allButLastDo: [ :gift | verseStream << gift << ', ' ].
		day > 1 ifTrue: [ verseStream << 'and '  ].
		verseStream << (gifts last) << '.' ]
]

{ #category : #accessing }
TwelveDays >> reciteStartVerse: start endVerse: end [ 
	"Answers a list containing all verses of the song between `start` and `end`."
	
	^ (start to: end) collect: [ :day | self class verseForDay: day ]
]
