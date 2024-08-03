Class {
	#name : #TwoFer,
	#superclass : #Object,
	#category : #'Exercise@TwoFer'
}

{ #category : #accessing }
TwoFer >> who [
	^ self who: 'you'.
]

{ #category : #accessing }
TwoFer >> who: aString [ 
	^ String streamContents: [ :aStream | aStream << 'One for ' << aString << ', one for me.'].
]
