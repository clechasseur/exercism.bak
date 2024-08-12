Extension { #name : #Number }

{ #category : #'*Exercise@SumOfMultiples' }
Number >> isDivisibleByAny: numbers [
	"Answers whether the receiver is divisible by at least one of the given numbers."
	
	^ numbers anySatisfy: [ :n | self isDivisibleBy: n ]
]

{ #category : #'*Exercise@SumOfMultiples' }
Number >> toExcluding: stop [
	"Answers an Interval between the receiver and the given stop, exclusive, by increment of 1."
	
	^ self toExcluding: stop by: 1
]

{ #category : #'*Exercise@SumOfMultiples' }
Number >> toExcluding: stop by: step [
	"Answers an Interval between the receiver up to stop, exclusive, incrementing by step."
	
	^ self to: (stop - 1) by: step
]
