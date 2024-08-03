"
I annotate Minesweeper fields with mine counts.

```
Minesweeper new annotateMinefield: #('   ' ' * ' '   ') >>> #('111' '1*1' '111')
```
"
Class {
	#name : #Minesweeper,
	#superclass : #Object,
	#category : #'Exercise@Minesweeper'
}

{ #category : #utilities }
Minesweeper >> annotateMinefield: aMinesList [
	"Given a minefield with mines and open spaces, fills spaces with mine counts as appropriate."

	^ (Minefield fromList: aMinesList) asAnnotatedList

]
