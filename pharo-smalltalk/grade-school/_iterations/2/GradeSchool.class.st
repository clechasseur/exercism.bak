"
I represent a grade school's student roster.
"
Class {
	#name : #GradeSchool,
	#superclass : #Object,
	#instVars : [
		'roster'
	],
	#category : #'Exercise@GradeSchool'
}

{ #category : #adding }
GradeSchool >> addStudents: students [ 
	"Adds students to the roster.
	Each element in the `students` collection must be a two-element list containing the name of the student along with their grade."
	
	students do: [ :student |
		(roster at: student second ifAbsentPut: [ SortedCollection new ])
			add: student first ].
]

{ #category : #accessing }
GradeSchool >> desiredGrade: grade [
	"Returns all students in the desired grade.
	Students are ordered by name (ascending)."
	
	^ (roster at: grade ifAbsent: #()) asArray
]

{ #category : #initialization }
GradeSchool >> initialize [
	roster := IdentityDictionary new.
]

{ #category : #accessing }
GradeSchool >> roster [
	"Returns the whole school roster as one list.
	Students will be ordered by grade (ascending) then name (ascending)."
	
	^ roster keys sorted flatCollect: [ :grade | self desiredGrade: grade ]
]
