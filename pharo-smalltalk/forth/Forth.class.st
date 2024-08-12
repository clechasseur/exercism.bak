"
I am an evaluator for a very simple subset of Forth.

Forth is a stack-based programming languages. A Forth program is composed of a series of _instructions_, or _words_. The following words are supported out-of-the-box:

* Numbers: push that number to the stack.
* `+`, `-`, `*`, `/`: Pop the top two values from the stack, perform the given arithmetic operation and pushes the value back on the stack.
* `DUP`: Duplicates the top stack value.
* `DROP`: Pops the top stack value, discarding it.
* `SWAP`: Swaps the top two stack values.
* `OVER`: Copies the second-from-the-top stack value to the top of the stack.

The `evaluateInstructions:` method evaluates the given instructions and returns the state of the stack after completion.

```
Forth new evaluateInstructions: #('1 2 +' ) >>> #(3 )
```

```
Forth new evaluateInstructions: #('2 4 * DUP +' ) >>> #(16 )
```

Words are evaluated case-insensitively.

```
Forth new evaluateInstructions: #('1 DUP dup dUp DuP' ) >>> #(1 1 1 1 1 )
```

It's possible to define custom words via `: word-name definition ;`. For example:

```
Forth new evaluateInstructions: #(': double 2 * ;'  '4 double' ) >>> #(8 )
```

It's possible to redefine existing words, even built-in words. The new definition applies only after the redefinition, however.

```
Forth new evaluateInstructions: #(': + * ;' ': foo 1 ;' ': bar foo ;' ': foo 2 ;' 'foo bar +' ) >>> #(2 )
```

Here, when `foo` is redefined to `2`, it does not affect the previous definition of `foo` that is used by the `bar` word; thus, `foo bar` evaluates to `2 1`, then `+` is evaluated to `*`, giving `2`.

It's possible to define words within word definitions. However, the interpreter stores all custom words in a single dictionary, so the sub-word definition will not be scoped to its outer definition only.

```
Forth new evaluateInstructions: #(': foo : bar 2 ; bar bar ;' 'foo bar' ) >>> #(2 2 2 )
```

Sub-definitions will be added to the dictionary when their terminal ';' is encountered - thus, they are added _before_ their outer definition. This allows seemingly-recursive word definitions to avoid being infinitely recursive.

```
Forth new evaluateInstructions: #(': foo : foo 2 ; foo ;' 'foo foo' ) >>> #(2 2 )
```
"
Class {
	#name : #Forth,
	#superclass : #Object,
	#classVars : [
		'BlankSpaceRegex'
	],
	#category : #'Exercise@Forth'
}

{ #category : #'private - evaluating' }
Forth class >> defineWord: aWordAssociation inInterpreter: interpreter [
	"Given an Association between a word and its associated instructions, defines the word in the given `ForthInterpreter`. Correctly handles sub-definitions recursively."
	
	| operations |
	operations := aWordAssociation value select: [ :instruction |
		instruction isString
			ifFalse: [ self defineWord: instruction inInterpreter: interpreter ];
			yourself ].
	
	interpreter defineWord: aWordAssociation key withOperations: operations.
]

{ #category : #'error handling' }
Forth class >> errorInvalidInstructions [
	"Raises an error to indicate that the instructions provided did are invalid. Example of invalid instructions:
	
	* A `;` is found outside of a custom word definition
	* Instructions ended without `;` while inside a custom word definition"
	
	self error: 'invalid instructions'
]

{ #category : #'class initialization' }
Forth class >> initialize [
	"Forth initialize"
	
	BlankSpaceRegex := '\s+' asRegex.
]

{ #category : #'private - parsing' }
Forth class >> matchInstruction: instruction ifColon: aColonBlock ifSemicolon: aSemicolonBlock otherwise: anOtherwiseBlock [
	"Matches the given instruction and depending on its type, calls the correct block."
	
	instruction = ':' ifTrue: [ ^ aColonBlock value ].
	instruction = ';' ifTrue: [ ^ aSemicolonBlock value ].
	^ anOtherwiseBlock value
]

{ #category : #'private - parsing' }
Forth class >> parse: instructionsStream [
	"Parses instructions in the given Stream of Strings and answers a collection of instructions. Each instruction will either be a String (e.g. one word or number) or an Association of custom word -> instructions.
	The parsing stops when a ';' is encountered, or if we reach the end of the stream. If ';' is encountered, it is consumed and removed from the stream before returning."
	
	| instructions done |
	instructions := OrderedCollection new.
	
	done := false.
	[ done ] whileFalse: [
		instructionsStream next
			ifNil: [ done := true ]
			ifNotNil: [ :instruction |
				self matchInstruction: instruction
					ifColon: [ instructions add: (self parseCustomWord: instructionsStream) ]
					ifSemicolon: [ done := true ]
					otherwise: [ instructions add: instruction ] ] ].
			
	^ instructions
]

{ #category : #'private - parsing' }
Forth class >> parseCustomWord: instructionsStream [
	"Given a stream of instructions as Strings, parses a custom word definition and answers an Association of the word name to its associated operations. The stream must be positioned so that the next token is the word name."
	
	^ instructionsStream next
		ifNil: [ self errorInvalidInstructions ]
		ifNotNil: [ :word | Association key: word value: (self parse: instructionsStream) ]
]

{ #category : #evaluating }
Forth >> evaluateInstructions: instructionsCollection [
	"Evaluates the given Forth instructions and answers the final state of the stack.
	Instructions must be provided as a collection of Strings, each String containing one or more instruction separated by whitespace.
	
	Note: each invocation of this method starts with an empty stack and no defined custom words (e.g., from scratch)."
	
	| interpreter |
	interpreter := ForthInterpreter new.
	instructionsCollection do: [ :instructions |
		| instructionsStream parsedInstructions |
		instructionsStream := (instructions splitOn: BlankSpaceRegex) readStream.
		parsedInstructions := self class parse: instructionsStream.
	
		"If there are leftover instructions in the stream, it means we encountered an unmatched ';'."
		instructionsStream atEnd ifFalse: [ self class errorInvalidInstructions ].
	
		parsedInstructions do: [ :instruction |
			instruction isString
				ifTrue: [ interpreter evaluate: instruction ]
				ifFalse: [ self class defineWord: instruction inInterpreter: interpreter ] ] ].
		
	^ interpreter stack
]
