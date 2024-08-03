"
I can determine if numbers are Armstrong numbers.

An Armstrong number in base B is a number that is equal to the sum of its digits, each raised to the power of its number of digits. For example, `153` is an Armstrong number in base 10 because `153 = 1^3 + 5^3 + 3^3 = 1 + 125 + 27 = 153`.

```
ArmstrongNumbers new isArmstrongNumber: 10 >>> false
```

```
ArmstrongNumbers new isArmstrongNumber: 9474 >>> true
```
"
Class {
	#name : #ArmstrongNumbers,
	#superclass : #Object,
	#category : #'Exercise@ArmstrongNumbers'
}

{ #category : #testing }
ArmstrongNumbers >> isArmstrongNumber: number [ 
	"Answers whether the given number is an Armstrong number in base 10. See class description for details."
	
	^ self isArmstrongNumber: number inBase: 10
]

{ #category : #testing }
ArmstrongNumbers >> isArmstrongNumber: number inBase: base [
	"Answers whether the given number is an Armstrong number in the given base. See class description for details."
	
	| numDigits remaining sum |
	numDigits := number numberOfDigitsInBase: base.
	remaining := number.
	sum := 0.
	
	[ remaining > 0 ] whileTrue: [
		| digit |
		digit := remaining \\ base.
		remaining := remaining // base.
		sum := sum + (digit ** numDigits) ].
	
	^ sum == number
]
