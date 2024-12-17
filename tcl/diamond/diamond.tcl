proc whitespace {n} {
    string repeat " " $n
}

variable letters [split "ABCDEFGHIJKLMNOPQRSTUVWXYZ" {}]
variable lettersRev [lreverse $letters]

proc ord {letter} {
    expr { [scan $letter %c] - [scan "A" %c] }
}

proc diamondLetters {letter} {
    variable letters
    variable lettersRev

    variable asc [lrange $letters 0 [ord $letter]]
    variable desc [lrange $lettersRev end-[expr { [ord $letter] - 1 }] end]
    list {*}$asc {*}$desc
}

proc diamondLine {letter lineSize} {
    if {$letter == "A"} {
        variable outer [whitespace [expr { ($lineSize - 1) / 2 }]]
        return [join [list $outer $letter $outer] {}]
    }

    variable innerSize [expr { [ord $letter] * 2 - 1 }]
    variable outerSize [expr { ($lineSize - $innerSize - 2) / 2 }]
    variable inner [whitespace $innerSize]
    variable outer [whitespace $outerSize]
    join [list $outer $letter $inner $letter $outer] {}
}

proc diamond {letter} {
    if {$letter == "A"} {
         return A
    }

    variable lineSize [expr { [ord $letter] * 2 + 1 }]
    variable upDownLetters [diamondLetters $letter]
    variable lines [lmap l $upDownLetters { diamondLine $l $lineSize }]
    join $lines "\n"
}
