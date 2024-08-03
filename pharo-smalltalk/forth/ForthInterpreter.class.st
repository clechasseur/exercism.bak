"
I am an interpreter for a very simple subset of Forth. See comments for the `Forth` class for details.
"
Class {
	#name : #ForthInterpreter,
	#superclass : #Object,
	#instVars : [
		'stack',
		'words'
	],
	#category : #'Exercise@Forth'
}

{ #category : #private }
ForthInterpreter class >> divide: numerator by: denominator [
	"Performs a division, checking for 'divide by zero' errors."
	
	denominator = 0 ifTrue: [ self errorDivideByZero ].
	^ numerator // denominator
]

{ #category : #'error handling' }
ForthInterpreter class >> errorDivideByZero [
	"Raises a 'divide by zero' error"
	
	self error: 'divide by zero'
]

{ #category : #'error handling' }
ForthInterpreter class >> errorEmptyStack [
	"Raises an error because the stack is empty."
	
	self error: 'empty stack'
]

{ #category : #'error handling' }
ForthInterpreter class >> errorIllegalOperation [
	"Raises an error indicating an illegal operation. This happens when the user tries to redefine a number as a custom word."
	
	self error: 'illegal operation'
]

{ #category : #'error handling' }
ForthInterpreter class >> errorNeedTwoStackValues [
	"Raises an error because we need two values on the stack but there is only one."
	
	self error: 'only one value on the stack'
]

{ #category : #'error handling' }
ForthInterpreter class >> errorUndefinedOperation [
	"Raises an error indicating an undefined operation (e.g., an unknown word)."
	
	self error: 'undefined operation'
]

{ #category : #private }
ForthInterpreter >> biWord: aBlock [
	"Given a block taking two arguments, answers a block that pops the top two values from the stack, calls `aBlock` with the two values and pushes the result back on the stack.
	
	Note: the arguments are passed to `aBlock` in reverse order, so if the stack is `12 3` for example, `aBlock`'s first argument would be 12 and the second one would be 3."
	
	^ [ | a b |
		stack ifEmpty: [ self class errorEmptyStack ].
		b := stack pop.
		stack ifEmpty: [ self class errorNeedTwoStackValues ].
		a := stack pop.
		stack push: (aBlock value: a value: b). ]
]

{ #category : #evaluating }
ForthInterpreter >> defineWord: word withOperations: operations [
	"Defines a new word as the given series of operations. If the word is already defined, it is overwritten.
	Operations need to be defined as a collection of Strings.
	Raises an error if `word` is a number, since numbers cannot be redefined.
	Raises an error if any operation is not defined."
	
	| ops |
	word asInteger ifNotNil: [ self class errorIllegalOperation ].
	
	"We need to copy the operations here and capture them in the block so that if any operation is later redefined, we use the proper version."
	ops := operations collect: [ :op | self wordBlock: op ].
	words
		at: word
		asUppercase put: [ ops do: #value ].
]

{ #category : #private }
ForthInterpreter >> drop [
	"Answers a block implementing the `DROP` word."
	
	^ [ stack
			ifEmpty: [ self class errorEmptyStack ]
			ifNotEmpty: [ stack pop ]. ]
]

{ #category : #private }
ForthInterpreter >> dup [
	"Answers a block implementing the `DUP` word."
	
	^ [ stack
			ifEmpty: [ self class errorEmptyStack ]
			ifNotEmpty: [ stack push: stack top ]. ]
]

{ #category : #evaluating }
ForthInterpreter >> evaluate: word [
	"Evaluates the given word and modifies our stack accordingly."
	
	(self wordBlock: word) value.
]

{ #category : #initialization }
ForthInterpreter >> initialize [
	stack := Stack new.
	words := {
		'+' -> (self biWord: [ :a :b | a + b ]) .
		'-' -> (self biWord: [ :a :b | a - b ]) .
		'*' -> (self biWord: [ :a :b | a * b ]) .
		'/' -> (self biWord: [ :a :b | self class divide: a by: b ]) .
		'DUP' -> (self dup) .
		'DROP' -> (self drop) .
		'SWAP' -> (self swap) .
		'OVER' -> (self over)
	} asDictionary.
]

{ #category : #private }
ForthInterpreter >> over [
	"Answers a block implementing the `OVER` word."
	
	^ [ | a b |
		stack ifEmpty: [ self class errorEmptyStack ].
		a := stack pop.
		stack ifEmpty: [ self class errorNeedTwoStackValues ].
		b := stack top.
		stack push: a.
		stack push: b. ]
]

{ #category : #accessing }
ForthInterpreter >> stack [
	"Answers an Array containing the values currently on the stack."
	
	"Note: Stack maintains its list in reverse, so we have to reverse it again here."
	^ stack asArray reversed
]

{ #category : #private }
ForthInterpreter >> swap [
	"Answers a block implementing the `SWAP` word."
	
	^ [ | a b |
		stack ifEmpty: [ self class errorEmptyStack ].
		a := stack pop.
		stack ifEmpty: [ self class errorNeedTwoStackValues ].
		b := stack pop.
		stack push: a.
		stack push: b. ]
]

{ #category : #'private - accessing' }
ForthInterpreter >> wordBlock: word [
	"Answers the block to use to evaluate the given word. Raises an error if the word is not defined.
	`word` is case-sensitive.
	If `word` is a number, returns a block that pushes that number to the stack."
	
	^ word asInteger
		ifNotNil: [ :number | [ stack push: number ] ]
		ifNil: [ words at: word asUppercase ifAbsent: [ self class errorUndefinedOperation ] ] 
]
