"
I convert numbers into funny raindrop sounds.

```
Raindrops new convertNumber: 4 >>> '4'
```

```
Raindrops new convertNumber: 105 >> 'PlingPlangPlong'
```
"
Class {
	#name : #Raindrops,
	#superclass : #Object,
	#classVars : [
		'SoundMap'
	],
	#category : #'Exercise@Raindrops'
}

{ #category : #'class initialization' }
Raindrops class >> initialize [
	"Raindrops initialize"

	SoundMap := Dictionary newFromKeys: #(3 5 7) andValues: { 'Pling' . 'Plang' . 'Plong' }.
]

{ #category : #raindrops }
Raindrops >> convertNumber: aNumber [
	"Convers aNumber into corresponding raindrop sounds."

	| sounds |
	sounds := (SoundMap
					associationsSelect: [ :assoc | aNumber \\ (assoc key) = 0 ])
					collect: [ :assoc | assoc value ].
	
	^ sounds isEmpty
			ifTrue: [ aNumber asString ]
			ifFalse: [ '' join: sounds ]
]
