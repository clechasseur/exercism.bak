"
I reverse strings, because reasons.
"
Class {
	#name : #ReverseString,
	#superclass : #Object,
	#category : #'Exercise@ReverseString'
}

{ #category : #utilities }
ReverseString >> reverseValue: aString [
	"Reverses the given string."
	
	^ String new: aString size streamContents: [ :stream |
		aString size to: 1 by: -1 do: [ :i | stream << (aString at: i) ] ]
]
