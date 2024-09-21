"
I can be used to detect isograms.

An isogram is a word or phrase where letters are never repeated, however spaces and hyphens are allowed more than once.

```
Isogram new isIsogramPhrase: 'hello world' >>> false
```

```
Isogram new isIsogramPhrase: 'the man ki' >>> true
```
"
Class {
	#name : #Isogram,
	#superclass : #Object,
	#category : #'Exercise@Isogram'
}

{ #category : #utilities }
Isogram >> isIsogramPhrase: aString [ 
	"Determines if the given word or phrase is an isogram."
	
	| cleaned |
	cleaned := aString onlyLetters asUppercase.
	^ cleaned asSet size = cleaned size
]
