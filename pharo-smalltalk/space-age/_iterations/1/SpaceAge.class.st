"
I calculate the age of someone on various planets of the solar system, in Earth years.

```
SpaceAge new ageOnPlanet: 'Venus' at: 1000234 >>> 0.05152087775322249
```

```
SpaceAge new ageOnPlanet: 'Earth' at: 31557600 >>> 1.0
```
"
Class {
	#name : #SpaceAge,
	#superclass : #Object,
	#classVars : [
		'EarthYearInSeconds',
		'OrbitalPeriods'
	],
	#category : #'Exercise@SpaceAge'
}

{ #category : #'class initialization' }
SpaceAge class >> initialize [
	"SpaceAge initialize"

	EarthYearInSeconds := 31557600.0.
	
	OrbitalPeriods := {
		'Mercury' -> 0.2408467.
		'Venus' -> 0.61519726.
		'Earth' -> 1.0.
		'Mars' -> 1.8808158.
		'Jupiter' -> 11.862615.
		'Saturn' -> 29.447498.
		'Uranus' -> 84.016846.
		'Neptune' -> 164.79132.
	} asDictionary.
]

{ #category : #'space age' }
SpaceAge >> ageOnPlanet: planetName at: seconds [
	"Given a number of seconds, returns the age on the corresponding planet, in Earth years."

	^ seconds / EarthYearInSeconds / (OrbitalPeriods at: planetName)
]
