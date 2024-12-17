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
        variable dice [list [roll] [roll] [roll] [roll]]
        set dice [lsort -integer $dice]
        set dice [lrange $dice 1 3]
        ::tcl::mathop::+ {*}$dice
    }

    proc character {} {
        variable constitution [ability]
        variable hitpoints [expr { 10 + [modifier $constitution] }]
        dict create strength [ability] dexterity [ability] constitution $constitution intelligence [ability] wisdom [ability] charisma [ability] hitpoints $hitpoints
    }
}
