"
I can compute the sum of multiples. #captainobvious
"
Class {
	#name : #SumOfMultiples,
	#superclass : #Object,
	#category : #'Exercise@SumOfMultiples'
}

{ #category : #arithmetic }
SumOfMultiples >> sumFactors: factors limit: limit [ 
	"Finds all unique multiples of the given factors under the provided limit and answers their sum.
	Any factor equal to 0 is ignored.
	Answers 0 if all factors are at or above the limit."
	
	"SumOfMultiples new sumFactors: #(3 0 5 ) limit: 10 >>> 23"
	
	"SumOfMultiples new sumFactors: #(3 0 5 ) limit: 2 >>> 0"
	
	| range multiples |
	
	range := 1 toExcluding: limit.
	multiples := range selectMultiplesOfAny: factors.
	^ multiples sumNumbers
]
