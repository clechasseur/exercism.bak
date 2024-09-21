"
# Acronym

I abbreviate phrases into uppercase acronyms.

```
Acronym new abbreviatePhrase: 'portable network graphics' >>> 'PNG'
```
"
Class {
	#name : #Acronym,
	#superclass : #Object,
	#instVars : [
		'rxWord'
	],
	#category : #'Exercise@Acronym'
}

{ #category : #utilities }
Acronym >> abbreviatePhrase: aPhrase [
	"Abbreviates the given phrase, generating the corresponding acronym."
	
	^ '' join: (self wordMatcher matchesIn: aPhrase collect: [ :aMatch | aMatch first asUppercase ])
]

{ #category : #initialization }
Acronym >> initialize [
	rxWord := RxParser new parse: '[A-Za-z][A-Za-z'']*'.
]

{ #category : #private }
Acronym >> wordMatcher [
	^ RxParser preferredMatcherClass for: rxWord
]
