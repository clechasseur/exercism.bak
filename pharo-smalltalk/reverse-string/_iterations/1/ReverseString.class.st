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
	
	| reverse size |
	reverse := aString copy.
	size := reverse size.
	
	1 to: size // 2 do: [ :i | reverse swap: i with: size - i + 1 ].
	^ reverse
]
