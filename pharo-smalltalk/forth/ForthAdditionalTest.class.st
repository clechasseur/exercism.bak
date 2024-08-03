Class {
	#name : #ForthAdditionalTest,
	#superclass : #ExercismTest,
	#instVars : [
		'forthCalculator'
	],
	#category : #'Exercise@Forth'
}

{ #category : #config }
ForthAdditionalTest class >> includeInSolution [
	^ true
]

{ #category : #running }
ForthAdditionalTest >> setUp [
	super setUp.
	forthCalculator := Forth new.
]

{ #category : #tests }
ForthAdditionalTest >> test01_CanDefineWordsWithinWordDefinitions [
	"Additional test: validate that word definitions can appear within a word definition."

	| result |

	result := forthCalculator evaluateInstructions: #(': foo : bar 2 ; bar bar ;' 'foo bar' ) .
	self assert: result equals: #(2 2 2 )
]

{ #category : #tests }
ForthAdditionalTest >> test02_InnerWordDefinitionsAreSavedBeforeOuterWordDefinitions [
	"Additional test: validate that inner word definitions (those found within another definition) are saved before their outer definition, thus allowing seemingly-recursive word definitions to work."

	| result |

	result := forthCalculator evaluateInstructions: #(': foo : foo 2 ; foo ;' 'foo foo' ) .
	self assert: result equals: #(2 2 )
]
