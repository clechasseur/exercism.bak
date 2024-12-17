namespace eval dnd {
    namespace export character ability modifier
    namespace ensemble create

    proc roll {} {
        expr { int(rand() * 6) + 1 }
    }

    proc modifier {score} {
        expr { ($score / 2) - 5 }
    }

    proc ability {} {
        ::tcl::mathop::+ {*}[lrange [lsort -integer [list [roll] [roll] [roll] [roll]]] 1 3]
    }

    proc character {} {
        variable constitution [ability]
        variable hitpoints [expr { 10 + [modifier $constitution] }]
        dict create strength [ability] dexterity [ability] constitution $constitution intelligence [ability] wisdom [ability] charisma [ability] hitpoints $hitpoints
    }
}
