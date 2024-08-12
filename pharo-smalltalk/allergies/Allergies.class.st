"
I can determine what a person is allergic to, given their allergy score.

Known allergens with their corresponding allergy score:
- eggs (1)
- peanuts (2)
- shellfish (4)
- strawberries (8)
- tomatoes (16)
- chocolate (32)
- pollen (64)
- cats (128)

A person's allergy score is obtained by adding the score of all items they are allergic to.

```
Allergies new allergicToItem: 'eggs' forScore: 1 >>> true
```

```
Allergies new allergyListForScore: 1 >>> #('eggs')
```
"
Class {
	#name : #Allergies,
	#superclass : #Object,
	#classVars : [
		'Allergens'
	],
	#category : #'Exercise@Allergies'
}

{ #category : #'private - validations' }
Allergies class >> allergicToItemWithScore: allergenScore forAllergyScore: score [
	"Checks if a person with the given allergy score is allergic to an item with the given allergen score.
	If allergenScore is 0, returns false."
	
	^ allergenScore ~= 0 and: [ (score bitAt: allergenScore) = 1 ]
]

{ #category : #'class initialization' }
Allergies class >> initialize [
	"Allergies initialize"
	
	Allergens := {
		'eggs' .
		'peanuts' .
		'shellfish' .
		'strawberries' .
		'tomatoes' .
		'chocolate' .
		'pollen' .
		'cats'
	}.
]

{ #category : #validations }
Allergies >> allergicToItem: allergen forScore: score [
	"Checks if a person with the given allergy score is allergic to the given allergen."
	
	^ self class
		allergicToItemWithScore: (Allergens indexOf: allergen)
		forAllergyScore: score
]

{ #category : #validations }
Allergies >> allergyListForScore: score [ 
	"Returns the list of allergens to which a person with the given allergy score is allergic to."
	
	^ Allergens
		withIndexSelect: [ :each :i | self class allergicToItemWithScore: i forAllergyScore: score ]
]
