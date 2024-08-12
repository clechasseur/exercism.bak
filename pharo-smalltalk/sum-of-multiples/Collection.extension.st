Extension { #name : #Collection }

{ #category : #'*Exercise@SumOfMultiples' }
Collection >> selectMultiplesOfAny: numbers [
	"Answers a collection selecting all elements of the receiver that are divisible by any of the given numbers. See #isDivisibleByAny: for more information."
	
	^ self select: [ :n | n isDivisibleByAny: numbers ]
]
