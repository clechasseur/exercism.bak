Class {
	#name : #ClockAdditionalTest,
	#superclass : #ExercismTest,
	#category : #'Exercise@Clock'
}

{ #category : #config }
ClockAdditionalTest class >> includeInSolution [
	^ true
]

{ #category : #tests }
ClockAdditionalTest >> test01_ClocksSupportStorageInSet [
	"Additional test: because Clock implements `hash`, it can be stored in a Set or Dictionary."
	| clock1 clock2 set |
	
	clock1 := Clock hour: 1 minute: 20.
	clock2 := Clock hour: 1 minute: 20.
	
	set := Set with: clock1 with: clock2.
	self assert: set size equals: 1.
	
	clock2 := Clock hour: 2 minute: 30.
	set add: clock2.
	self assert: set size equals: 2
]

{ #category : #tests }
ClockAdditionalTest >> test02_ComparisonLessThanIsSupported [
	"Additional test: operator #<"
	| clock_0314 clock_1327 |
	
	clock_0314 := Clock hour: 3 minute: 14.
	clock_1327 := Clock hour: 13 minute: 27.
	
	self assert: clock_0314 < clock_1327.
	self deny: clock_1327 < clock_1327.
	self deny: clock_1327 < clock_0314
]

{ #category : #tests }
ClockAdditionalTest >> test03_ComparisonLessThanOrEqualToIsSupported [
	"Additional test: operator #<="
	| clock_0314 clock_1327 |
	
	clock_0314 := Clock hour: 3 minute: 14.
	clock_1327 := Clock hour: 13 minute: 27.
	
	self assert: clock_0314 <= clock_1327.
	self assert: clock_1327 <= clock_1327.
	self deny: clock_1327 <= clock_0314
]

{ #category : #tests }
ClockAdditionalTest >> test04_ComparisonGreaterThanIsSupported [
	"Additional test: operator #>"
	| clock_0314 clock_1327 |
	
	clock_0314 := Clock hour: 3 minute: 14.
	clock_1327 := Clock hour: 13 minute: 27.
	
	self deny: clock_0314 > clock_1327.
	self deny: clock_1327 > clock_1327.
	self assert: clock_1327 > clock_0314
]

{ #category : #tests }
ClockAdditionalTest >> test06_ComparisonGreaterThanOrEqualToIsSupported [
	"Additional test: operator #>="
	| clock_0314 clock_1327 |
	
	clock_0314 := Clock hour: 3 minute: 14.
	clock_1327 := Clock hour: 13 minute: 27.
	
	self deny: clock_0314 >= clock_1327.
	self assert: clock_1327 >= clock_1327.
	self assert: clock_1327 >= clock_0314
]

{ #category : #tests }
ClockAdditionalTest >> test07_Midnight [
	"Additional test: #midnight"
	| clock1 clock2 |
	
	clock1 := Clock midnight.
	clock2 := Clock hour: 0 minute: 0.
	self assert: clock1 equals: clock2
]

{ #category : #tests }
ClockAdditionalTest >> test08_Noon [
	"Additional test: #noon"
	| clock1 clock2 |
	
	clock1 := Clock noon.
	clock2 := Clock hour: 12 minute: 0.
	self assert: clock1 equals: clock2
]
